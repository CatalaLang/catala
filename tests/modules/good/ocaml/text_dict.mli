[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

module Mod_def = Mod_def
module Text = Text

module Text_dict : CatalaType

(** Toplevel definition empty *)
val empty : Text_dict.t

(** Toplevel definition store *)
val store : Text_dict.t -> Text.Text.t -> Mod_def.Mod_def.t -> Text_dict.t

(** Toplevel definition find *)
val find : Text_dict.t -> Text.Text.t -> Mod_def.Mod_def.t Optional.t
