open Catala_utils

type execution = { num_constraints : int }

type t = {
  total_tests : int;
  executions : execution list;
  num_unsat : int;
  num_soft_unsat : int;
  num_soft_sat : int;
  soft_sats : int String.Map.t;
  soft_unsats : int String.Map.t;
}

let empty : t =
  {
    total_tests = 0;
    executions = [];
    num_unsat = 0;
    num_soft_unsat = 0;
    num_soft_sat = 0;
    soft_sats = String.Map.empty;
    soft_unsats = String.Map.empty;
  }

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

let add_stat_exec (stats : t) num_constraints : t =
  { stats with executions = { num_constraints } :: stats.executions }

let format (fmt : Format.formatter) (stats : t) =
  let open Format in
  let max_constraints =
    List.fold_left
      (fun acc exe -> max acc exe.num_constraints)
      0 stats.executions
  in
  fprintf fmt "max constraints: %d" max_constraints
