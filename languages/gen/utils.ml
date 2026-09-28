(* This file is part of the Catala compiler, a specification language
   for tax and social benefits computation rules. Copyright (C) 2026
   Inria, contributors: Vincent Botbol <vincent.botbol@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you
   may not use this file except in compliance with the License. You
   may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
   implied. See the License for the specific language governing
   permissions and limitations under the License. *)

let utf8_seq s =
  let n = String.length s in
  let rec aux i () =
    if i >= n then Seq.Nil
    else
      let decoded = String.get_utf_8_uchar s i in
      Seq.Cons
        ( Uchar.utf_decode_uchar decoded,
          aux (i + Uchar.utf_decode_length decoded) )
  in
  aux 0

let contents filename =
  let with_in_channel ?(bin = true) filename f =
    let oc = (if bin then open_in_bin else open_in) filename in
    let finally () = close_in oc in
    match f oc with
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      finally ();
      Printexc.raise_with_backtrace e bt
    | r ->
      finally ();
      r
  in

  with_in_channel filename (fun ic ->
      really_input_string ic (in_channel_length ic))
