(* Three-way minimum *)
let minimum a b c = min a (min b c)

(** Computes the levenshtein distance between two strings, used to provide error
    messages suggestions *)
let levenshtein_distance (s : string) (t : string) : int =
  let m = String.length s and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between the
     first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m + 1) (n + 1) 0 in

  for i = 0 to m do
    d.(i).(0) <- i
    (* the distance of any first string to an empty second string *)
  done;
  for j = 0 to n do
    d.(0).(j) <- j
    (* the distance of any second string to an empty first string *)
  done;

  for j = 1 to n do
    for i = 1 to m do
      if s.[i - 1] = t.[j - 1] then d.(i).(j) <- d.(i - 1).(j - 1)
        (* no operation required *)
      else
        d.(i).(j) <-
          minimum
            (d.(i - 1).(j) + 1) (* a deletion *)
            (d.(i).(j - 1) + 1) (* an insertion *)
            (d.(i - 1).(j - 1) + 1) (* a substitution *)
    done
  done;

  d.(m).(n)

(*On crée la liste des distances minimales, c'est à dire tous les couples (>=1)
  qui partagent la distance minimale*)
let suggestion_minimum_levenshtein_distance_association
    (l : string list)
    (mot : string) : string list =
  let rec insertion ((x, y) : int * string) (l : (int * string) list) :
      (int * string) list =
    match l with
    | (current_x, current_y) :: t ->
      if x <= current_x then (x, y) :: l
        (*égalité car insertion du dernier au premier élément*)
      else (current_x, current_y) :: insertion (x, y) t
    | [] -> l @ [x, y]
  in
  (*on associe à chaque string de l sa distance de levenshtein avec un mot
    commun. La liste en sortie est triée (principe premier arrivé, premier
    inscrit)*)
  (*sauf accumulateur*)
  let rec levenshtein_distance_association (l' : string list) (mot' : string) :
      (int * string) list =
    match l' with
    | h :: t ->
      insertion
        (levenshtein_distance h mot', h)
        (levenshtein_distance_association t mot')
    | [] -> []
  in
  let final_list = levenshtein_distance_association l mot in
  match final_list with
  | h :: _ ->
    (*on filtre les minimums et on récupère les strings*)
    List.map snd (List.filter (fun (x, _) -> x == fst h) final_list)
    (*< impossible car déjà la liste est déjà triée*)
  | [] -> []
