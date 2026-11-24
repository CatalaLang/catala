open Catala_utils

(* TODO: quel temps manque ? compter le nombre d'evals *)
(* GC: pic mémoire alloué ? Z3 incrémental le dit ? *)
type time = float
type period = { start : time; stop : time }
type step = string * period

type execution = {
  steps : step list;
  total_time : period;
  num_constraints : int;
}

type t = {
  total_tests : int;
  total_time : period;
  steps : step list;
  executions : execution list;
  num_unsat : int;
  num_soft_unsat : int;
  num_soft_sat : int;
  soft_sats : int String.Map.t;
  soft_unsats : int String.Map.t;
}

let start_period () : period =
  let start = Sys.time () in
  { start; stop = nan }

let stop_period p : period =
  let stop = Sys.time () in
  { p with stop }

let init () : t =
  let total_time = start_period () in
  {
    total_tests = 0;
    total_time;
    steps = [];
    executions = [];
    num_unsat = 0;
    num_soft_unsat = 0;
    num_soft_sat = 0;
    soft_sats = String.Map.empty;
    soft_unsats = String.Map.empty;
  }

(* to reorganize *)

let incr_sat group stats =
  {
    stats with
    soft_sats =
      String.Map.update group
        (fun n -> Some (Option.value ~default:0 n + 1))
        stats.soft_sats;
  }

let incr_unsat group stats =
  {
    stats with
    soft_unsats =
      String.Map.update group
        (fun n -> Some (Option.value ~default:0 n + 1))
        stats.soft_unsats;
  }

let format_soft_sats fmt stats =
  let open Format in
  fprintf fmt "@[<v 2>sat groups:@ %a@]"
    (String.Map.format_bindings_i ~pp_sep:pp_print_cut (fun fmt _pp_k name n ->
         fprintf fmt "%n %s sat group" n name))
    stats.soft_sats

let format_soft_unsats fmt stats =
  let open Format in
  fprintf fmt "@[<v 2>unsat groups:@ %a@]"
    (String.Map.format_bindings_i ~pp_sep:pp_print_cut (fun fmt _pp_k name n ->
         fprintf fmt "%n %s unsat group" n name))
    stats.soft_unsats

let incr_tests stats = { stats with total_tests = succ stats.total_tests }
let start_step message : step = message, start_period ()
let stop_step ((msg, p) : step) : step = msg, stop_period p

let start_exec num_constraints : execution =
  let total_time = start_period () in
  { steps = []; total_time; num_constraints }

let add_exec_step (e : execution) step : execution =
  { e with steps = step :: e.steps }

let stop_exec (e : execution) : execution =
  let total_time = stop_period e.total_time in
  { e with total_time }

let add_stat_step (stats : t) (step : step) : t =
  { stats with steps = step :: stats.steps }

let add_stat_exec (stats : t) (e : execution) : t =
  { stats with executions = e :: stats.executions }

let stop stats : t =
  let total_time = stop_period stats.total_time in
  { stats with total_time }

let running_period stats : period = stop_period stats.total_time

let fold_execs (execs : execution list) : step list =
  let f (steps : step list) (exec : execution) =
    List.map2
      (fun (s, p) (s', p') ->
        assert (String.equal s s');
        s, { p with stop = p.stop +. p'.stop -. p'.start })
      steps exec.steps
  in
  match execs with
  | [] -> []
  | { steps; _ } :: execs -> List.fold_left f steps execs

module Print = struct
  open Format

  (* let ms (fmt : formatter) (t : time) = *)
  (*   let milli : int = int_of_float (t *. 1000.) in *)
  (*   pp_print_int fmt milli; *)
  (*   pp_print_string fmt " ms" *)

  let sec (fmt : formatter) (t : time) =
    fprintf fmt "%.3f" t;
    pp_print_string fmt " s"

  let period (fmt : formatter) (p : period) = sec fmt (p.stop -. p.start)

  let step (fmt : formatter) ((msg, p) : step) =
    fprintf fmt "%s: %a" msg period p

  (* let itemize (bullet : string) (ppf : formatter -> 'a -> unit) (fmt :
       formatter) (x : 'a) = fprintf fmt "%s %a" bullet ppf x *)

  let steps (fmt : formatter) (l : step list) =
    let l = List.rev l in
    pp_print_list ~pp_sep:pp_print_cut step fmt l

  let executions (fmt : formatter) (execs : execution list) =
    let folded = fold_execs execs |> List.rev in
    let max_constraints =
      List.fold_left (fun acc exe -> max acc exe.num_constraints) 0 execs
    in
    pp_print_list ~pp_sep:pp_print_cut step fmt folded;
    pp_print_cut fmt ();
    fprintf fmt "max constraints: %n" max_constraints
end

let print (fmt : Format.formatter) (stats : t) =
  let open Format in
  fprintf fmt "General steps:@\n@[<v 2>  %a@]@\n" Print.steps stats.steps;
  fprintf fmt "After %n execution steps:@\n@[<v 2>  %a@]@\n"
    (List.length stats.executions)
    Print.executions stats.executions;
  fprintf fmt "Total concolic time: %a" Print.period stats.total_time
