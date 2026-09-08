(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils

type module_info = {
  name : string Mark.pos;
  item : Scan.item;
  (* extra_items: Scan.item list; (* e.g. included files *) *)
  targets : String.Set.t;
}

module G = struct
  include Graph.Persistent.Digraph.ConcreteBidirectional (struct
    include String

    let hash = Hashtbl.hash
  end)

  (* Attributes for Graphviz.Dot *)
  let graph_attributes _ = []

  let default_vertex_attributes _ =
    [`Fontname "sans"; `Shape `Box; `Style `Filled; `Fillcolor 0xffffff]

  let vertex_name v = String.quote v
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end [@warning "-32"]

let stdlib_target_name = "libcatala"

type info = {
  var_bindings : Var.bindings;
  modules_map : module_info String.Map.t;
  targets_map : Clerk_config.target String.Map.t;
  target_deps : Clerk_config.target -> String.Set.t;
  linking_deps : Scan.item -> string list;
  inclusion_map : Scan.item String.Map.t;
}

let empty_info =
  {
    var_bindings = [];
    modules_map = String.Map.empty;
    targets_map = String.Map.empty;
    target_deps = (fun t -> raise (String.Map.Not_found t.tname));
    linking_deps = (fun m -> raise (String.Map.Not_found m.file_name));
    inclusion_map = String.Map.empty;
  }

let inclusion_map items =
  let direct_map =
    Seq.fold_left
      (fun map it ->
        List.fold_left
          (fun map (f, _pos) -> String.Map.add f it map)
          map it.Scan.included_files)
      String.Map.empty items
  in
  let rec find it map =
    match String.Map.find_opt it.Scan.file_name map with
    | None -> it
    | Some parent -> find parent map
  in
  String.Map.fold
    (fun file it map -> String.Map.add file (find it map) map)
    direct_map direct_map

let organise_modules ~config ~var_bindings items =
  let module_g, modmap, _stdlib_modules =
    let modmap, stdlib_modules =
      Seq.fold_left
        (fun (modmap, stdlib_modules) item ->
          match item.Scan.module_def with
          | None -> modmap, stdlib_modules
          | Some (modname, pos) ->
            let info =
              {
                name = modname, pos;
                item;
                targets =
                  (if item.Scan.is_stdlib then
                     String.Set.singleton stdlib_target_name
                   else String.Set.empty);
              }
            in
            let modmap =
              String.Map.update modname
                (function
                  | None -> Some info
                  | Some conflict ->
                    (* Note: until now this was allowed. However, targets
                       select their contents by module name only, so this
                       could only be for local modules ? We could switch to
                       UIDs to support this, or somehow namespace the
                       modules by dir.
                       We need to implement something else than picking
                       randomly, in any case *)
                    Message.error ~pos
                      ~extra_pos:["", Mark.get conflict.name]
                      "Conflicting module name @{<blue>%s@}" modname)
                modmap
            in
            ( modmap,
              if item.Scan.is_stdlib then modname :: stdlib_modules
              else stdlib_modules ))
        (String.Map.empty, []) items
    in
    let mg =
      String.Map.fold
        (fun modname info mg ->
          List.fold_left
            (fun g (m, pos) ->
              if String.Map.mem m modmap then G.add_edge g modname m
              else
                Message.error ~pos
                  "Missing dependency in@ @{<blue>%s@}:@ module@ @{<blue>%s@}@ \
                   not@ found."
                  modname m)
            (G.add_vertex mg modname) info.item.used_modules)
        modmap G.empty
    in
    mg, modmap, stdlib_modules
  in
  let stdlib_target =
    {
      Clerk_config.tname = stdlib_target_name;
      tmodules = ["Stdlib_en"; "Stdlib_fr"];
      ttests = [];
      backends = List.map snd (Clerk_config.registered_backends ());
      dependencies = [];
    }
  in
  let target_g, modmap, tmap =
    List.fold_left
      (fun (tg, modmap, tmap) t ->
        let tname = t.Clerk_config.tname in
        let tmap =
          String.Map.update tname
            (function
              | None -> Some t
              | Some _ ->
                Message.error
                  "Conflicting target name: @{<yellow>%s@} is defined twice"
                  tname)
            tmap
        in
        let modmap =
          List.fold_left
            (fun modmap m ->
              String.Map.update m
                (function
                  | Some i ->
                    Some { i with targets = String.Set.add tname i.targets }
                  | None ->
                    Message.error
                      "Target @{<yellow>%s@} is declared to use module \
                       @{<blue>%s@}, which was not found"
                      tname m)
                modmap)
            modmap t.tmodules
        in
        let tg = G.add_vertex tg tname in
        let tg =
          List.fold_left
            (fun tg dep ->
              if
                not
                  (String.Map.mem dep tmap
                  || List.exists
                       (fun t -> t.Clerk_config.tname = dep)
                       config.Clerk_cli.file.targets)
              then
                Message.error
                  "Clerk target @{<yellow>%s@}@ lists@ @{<yellow>%s@}@ as@ \
                   dependency,@ but@ that@ target@ was@ not@ found."
                  tname dep;
              G.add_edge tg t.Clerk_config.tname dep)
            tg t.dependencies
        in
        tg, modmap, tmap)
      (G.empty, modmap, String.Map.empty)
      (stdlib_target
      :: List.map
           (fun t ->
             {
               t with
               Clerk_config.dependencies =
                 stdlib_target_name :: t.Clerk_config.dependencies;
             })
           config.file.targets)
  in
  let () =
    (* Check that a target's backend are a subset of its dependencies' *)
    G.iter_vertex
      (fun t ->
        let t_backends = (String.Map.find t tmap).backends in
        List.iter
          (fun t1 ->
            let t1_backends = (String.Map.find t1 tmap).backends in
            List.iter
              (fun bk ->
                if not (List.mem bk t1_backends) then
                  Message.error
                    "Target @{<yellow>%s@}@ is@ configured@ to@ support@ \
                     backend@ @{<cyan>%s@},@ but@ it@ depends@ on@ \
                     @{<yellow>%s@}@ which@ doesn't@ support@ it."
                    t
                    (Clerk_config.backend_name bk)
                    t1)
              t_backends)
          (G.succ target_g t))
      target_g
  in
  let module Op = Graph.Oper.P (G) in
  let print_dot oc =
    let explicit_targets = String.Map.map (fun info -> info.targets) modmap in
    fun modmap ->
      let copy_vertex ~filter g v1 v2 =
        let g = G.add_vertex g v2 in
        let g =
          G.fold_pred
            (fun w g -> if filter w then G.add_edge g w v2 else g)
            g v1 g
        in
        G.fold_succ
          (fun w g -> if filter w then G.add_edge g v2 w else g)
          g v1 g
      in
      let module_g = Op.transitive_reduction module_g in
      let module_g =
        (* Remove internal stdlib modules *)
        G.fold_vertex
          (fun v g ->
            (* Uncomment this instead to make the stdlib root modules appear *)
            (* if List.exists (fun v -> (String.Map.find v modmap).item.is_stdlib) (G.pred module_g v) *)
            if (String.Map.find v modmap).item.is_stdlib then
              G.remove_vertex g v
            else g)
          module_g module_g
      in
      let g, modmap, explicit =
        (* Duplicate modules that belong to multiple targets *)
        G.fold_vertex
          (fun v (g, modmap, explicit) ->
            let info = String.Map.find v modmap in
            if String.Set.cardinal info.targets <= 1 then
              let explicit =
                if
                  String.Set.is_empty
                    (String.Set.inter info.targets
                       (String.Map.find v explicit_targets))
                then explicit
                else String.Set.add v explicit
              in
              g, modmap, explicit
            else
              let g, modmap, explicit =
                String.Set.fold
                  (fun target (g, modmap, explicit) ->
                    let vn = v ^ " (" ^ target ^ ")" in
                    ( copy_vertex g v vn ~filter:(fun v ->
                          String.Set.exists
                            (fun t ->
                              List.mem t (target :: G.pred target_g target))
                            (String.Map.find v modmap).targets),
                      String.Map.add vn
                        { info with targets = String.Set.singleton target }
                        modmap,
                      if
                        String.Set.mem target
                          (String.Map.find v explicit_targets)
                      then String.Set.add vn explicit
                      else explicit ))
                  info.targets (g, modmap, explicit)
              in
              G.remove_vertex g v, modmap, explicit)
          module_g
          (module_g, modmap, String.Set.empty)
      in
      let module Dot = Graph.Graphviz.Dot (struct
        include G

        let get_subgraph v =
          match String.Set.choose_opt (String.Map.find v modmap).targets with
          | None -> None
          | Some target ->
            Some
              {
                Graph.Graphviz.DotAttributes.sg_name = String.to_id target;
                sg_attributes =
                  [
                    `Style `Filled;
                    `Style `Dashed;
                    `Fillcolor 0xffffaa;
                    `Label target;
                  ];
                sg_parent = None;
              }

        let vertex_attributes v =
          let color =
            if String.contains v '(' then 0xffaaaa
            else if String.Set.mem v explicit then 0xaaffff
            else 0xffffff
          in
          `Fillcolor color
          :: (if String.Set.mem v explicit then [`Shape `Box3d] else [])
      end) in
      Dot.output_graph oc g
  in
  let check_cycles label g =
    let module SCC = Graph.Components.Make (G) in
    let sccs = SCC.scc_list g in
    match List.find_opt (function [] | [_] -> false | _ -> true) sccs with
    | None | Some [] -> ()
    | Some (v :: vs) ->
      Message.error
        "@[<v>@[<v 4>@[Dependency between the following %s is cyclic:@]@,\
         %a@]@,\
         @,\
         @[<hov>The dependency graph in Dot format is available in@ \
         @{<bold;blue>%t@}.@]@]"
        label
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " depends on@,")
           (fun ppf v -> Format.fprintf ppf "@{<yellow>%s@}" v))
        ((v :: vs) @ [v])
        (let f = File.(config.file.global.build_dir / "modules.dot") in
         File.with_out_channel f (fun oc -> print_dot oc modmap);
         fun ppf -> Message.link ~target:(Message.file_url f) () ppf f)
  in
  check_cycles "targets" target_g;
  check_cycles "modules" module_g;
  let linking_deps =
    let module Topo = Graph.Topological.Make_stable (G) in
    fun item ->
      let depg =
        let rec add_vertex depg m =
          if G.mem_vertex depg m then depg
          else
            let depg = G.add_vertex depg m in
            List.fold_left
              (fun depg m1 ->
                let depg = add_vertex depg m1 in
                G.add_edge depg m m1)
              depg (G.succ module_g m)
        in
        List.fold_left add_vertex G.empty
          (List.map Mark.remove item.Scan.used_modules)
      in
      Topo.fold (fun m acc -> m :: acc) depg []
  in
  let target_g = Op.transitive_closure target_g in
  let module_g = Op.transitive_closure module_g in
  let leaves g =
    G.fold_vertex
      (fun t set -> if G.out_degree g t = 0 then String.Set.add t set else set)
      g String.Set.empty
  in
  let subgraph g set =
    if String.Set.is_empty set then G.empty
    else
      G.fold_vertex
        (fun v g -> if String.Set.mem v set then g else G.remove_vertex g v)
        g g
  in
  let modmap =
    String.Map.fold
      (fun m info new_modmap ->
        let dependents = G.pred module_g m in
        (* All the targets that effectively depend on m *)
        let targets =
          List.fold_left
            (fun targets dm ->
              String.Set.union targets (String.Map.find dm modmap).targets)
            info.targets dependents
        in
        let dep_target_graph = subgraph target_g targets in
        (* The ones in which m needs to be actually included (the others depend
           on them and will access it that way) *)
        let base_targets =
          String.Set.union (leaves dep_target_graph) info.targets
        in
        (* Message.debug "@[<h>Module @{<blue>%s@} (%s%a) to be attached to targets {%a}.@]"
         *   m (if String.Set.is_empty info.targets then "no explicit target" else "targets ")
         *   (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf s -> Format.fprintf ppf "@{<yellow>%s@}" s))
         *   (String.Set.elements info.targets)
         *   (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf s -> Format.fprintf ppf "@{<yellow>%s@}" s))
         *   (String.Set.elements base_targets); *)
        let has_tests m =
          let has_tests_item it =
            it.Scan.has_inline_tests || Lazy.force it.has_scope_tests > 0
          in
          has_tests_item (String.Map.find m modmap).item
          || Seq.exists
               (fun it ->
                 has_tests_item it
                 && List.exists (fun m1 -> Mark.remove m1 = m) it.used_modules)
               items
        in
        if
          String.Set.is_empty base_targets
          && (not (has_tests m))
          && not (List.exists has_tests (G.pred module_g m))
        then
          Message.warning
            "The module@ @{<blue>%s@}@ belongs@ to@ no@ target@ and@ appears@ \
             to@ be@ unused"
            m;
        let check_conflicts () =
          let _, conflict_targets =
            (* We allow a module to get attached to multiple targets
               (`base_targets`). However, this creates a conflict between any two
               indepenednt users of the same module. The fix would be to attach
               the given module to a target that is a shared dependency of the
               using targets *)
            String.Set.fold
              (fun t (seen, conflicts) ->
                let depend_on_t = String.Set.of_list (G.pred target_g t) in
                let clash =
                  leaves (subgraph target_g (String.Set.inter seen depend_on_t))
                in
                ( String.Set.union seen depend_on_t,
                  String.Set.union conflicts clash ))
              base_targets
              (String.Set.empty, String.Set.empty)
          in
          let conflict_err cflt =
            let bases =
              String.Set.inter
                (String.Set.of_list (cflt :: G.succ target_g cflt))
                base_targets
            in
            Message.error
              "@[<v>@[<hov>Module conflict error in@ target@ @{<yellow>%s@}:@ \
               module@ @{<blue>%s@}@ would@ be@ included@ multiple@ times.@]@,\
               @[<hov>The following targets independently@ include@ it@ \
               (either@ explicitely,@ or@ because@ one@ of@ their@ modules@ \
               uses@ it):@]@,\
              \    @[<v>%a@]@,\
               @,\
               @[<hov>@{<bold>Hint:@} @{<blue>%s@}@ should@ be@ included@ in@ \
               a@ unique@ target@ that@ is@ listed@ in@ the@ \
               @{<cyan>dependencies@}@ field@ of@ all@ targets@ that@ might@ \
               use@ it.@]@,\
               @,\
               @[<hov>The dependency graph in Dot format is available in@ \
               @{<bold;blue>%t@}.@]@]"
              cflt m
              (Format.pp_print_list (fun ppf t ->
                   Format.fprintf ppf "- @{<yellow>%s@}" t))
              (String.Set.elements bases)
              m
              (let f = File.(config.file.global.build_dir / "modules.dot") in
               File.with_out_channel f (fun oc -> print_dot oc new_modmap);
               fun ppf -> Message.link ~target:(Message.file_url f) () ppf f)
          in
          Option.iter conflict_err (String.Set.choose_opt conflict_targets)
        in
        check_conflicts ();
        String.Map.add m { info with targets = base_targets } new_modmap)
      modmap modmap
  in
  let tmap =
    String.Map.fold
      (fun m info tmap ->
        String.Set.fold
          (fun t tmap ->
            String.Map.update t
              (function
                | Some target ->
                  Some
                    {
                      target with
                      Clerk_config.tmodules = m :: target.Clerk_config.tmodules;
                    }
                | None -> assert false)
              tmap)
          info.targets tmap)
      modmap tmap
    |> String.Map.map (fun target ->
        {
          target with
          Clerk_config.tmodules =
            List.sort_uniq String.compare target.Clerk_config.tmodules;
          dependencies = G.succ target_g target.tname;
        })
  in
  if Catala_utils.Global.options.debug then (
    let f = File.(config.file.global.build_dir / "modules.dot") in
    File.with_out_channel f (fun oc -> print_dot oc modmap);
    Message.debug "Module graph available at @{<blue;bold>%a@}"
      (Message.link ~target:(Message.file_url f) ())
      f);
  let target_deps (t : Clerk_config.target) =
    G.succ target_g t.tname |> String.Set.of_list
  in
  {
    var_bindings;
    modules_map = modmap;
    targets_map = tmap;
    target_deps;
    linking_deps;
    inclusion_map = inclusion_map items;
  }

(* Returns the targets a module belongs to, or, failing that, the targets of its
   dependencies *)
let rec module_target_dependencies info m =
  if not (String.Set.is_empty m.targets) then m.targets
  else
    List.fold_left
      (fun targets (depname, _) ->
        let m1 = String.Map.find depname info.modules_map in
        String.Set.union targets (module_target_dependencies info m1))
      String.Set.empty m.item.used_modules

(* The backends for a given module are detected by analysing what clerk targets
   it belongs to *)
let module_backends info modname =
  let m = String.Map.find modname info.modules_map in
  if String.Set.is_empty m.targets then
    let all_backends = List.map snd (Clerk_config.registered_backends ()) in
    let dep_targets = module_target_dependencies info m in
    (* Intersection of the backends supported by the module deps *)
    String.Set.fold
      (fun t acc ->
        List.filter
          (fun bk1 -> List.mem bk1 acc)
          (String.Map.find t info.targets_map).Clerk_config.backends)
      dep_targets all_backends
  else
    (* Union of the backends supported by the module targets *)
    String.Set.fold
      (fun t acc ->
        List.fold_left
          (fun acc bk -> if List.mem bk acc then acc else bk :: acc)
          acc (String.Map.find t info.targets_map).Clerk_config.backends)
      m.targets []
