/*
  This module allows to deserialize log events from the [french_law] web API.

  NOTE: Following types are deserialized with the ReScript lib [decco] which
        seems to have the same encoding that [ppx_yojson_conv], the one used in the
        OCaml side to serialize them.
        But there is no guarantee that the encoding will continue to be the same.

  @see https://github.com/CatalaLang/catala/tree/master/french_law/ocaml#wrappers and
  @see https://github.com/CatalaLang/catala/blob/master/compiler/runtime.mli#L26-L185
  @see https://github.com/reasonml-labs/decco
  @see https://github.com/janestreet/ppx_yojson_conv
*/

/* Position of an event in a Catala file. */
@decco.decode
type sourcePosition = {
  filename: string,
  start_line: int,
  end_line: int,
  start_column: int,
  end_column: int,
  law_headings: array<string>,
}

module LoggedValue: {
  /* Catala runtime value. */
  @decco.decode
  type rec t =
    | Unit
    | Bool(bool)
    | Integer(int)
    | Money(float)
    | Decimal(float)
    | Date(string)
    | Duration(string)
    // The enum type followed the name of the enum constructor and its value.
    | Enum(list<string>, (string, t))
    // The struct type followed the list of its fields.
    | Struct(list<string>, list<(string, t)>)
    | Array(array<t>)
    // Functions -- which are ignored.
    | Unembeddable

  /*
    [loggedValueToString(val, tab)] returns the string representation of [val]
    indented of [tab] tabulations.
 */
  let loggedValueToString: (t, int) => string
}

/*
  There is two kinds of log events:
  - _raw_ ones are the events that are directly received from the web API.
  - _structured_ ones are the events built from the raw ones.
*/

module Raw: {
  /* Serialized raw event received directly from the [french_law] web API. */
  type eventSerialized = {
    eventType: string,
    information: array<string>,
    sourcePosition: Js.Nullable.t<sourcePosition>,
    loggedValueJson: string,
  }

  type eventType =
    | BeginCall
    | EndCall
    | VariableDefinition
    | DecisionTaken

  /* Deserialized raw event. */
  type event = {
    eventType: eventType,
    information: array<string>,
    sourcePosition: option<sourcePosition>,
    loggedValue: LoggedValue.t,
  }

  let deserializedEvents: array<eventSerialized> => array<event>
}

/* Serialized log event received directly from the [french_law] web API, in a [data] payload. */
type eventSerialized = {data: string}

/*
  Represents information about a name in the code -- i.e. variable name,
  subscope name, etc...

  It's a list of strings with a length varying from 2 to 3, where:

  - the first string is the name of the current scope -- starting with a
    capitalized letter [Scope_name],
  - the second string is either: the name of a scope variable or, the name of
    a subscope input variable -- [a_subscope_var.input_var]
  - the third string is either: a subscope name (starting with a capitalized
    letter [Subscope_name] or, the [input] (resp. [output]) string -- which
    corresponds to the input (resp. the output) of a function.
*/
@decco.decode
type information = list<string>

@decco.decode
type io_input =
  | NoInput
  | OnlyInput
  | Reentrant

@decco.decode
type io_log = {
  io_input: io_input,
  io_output: bool,
}

/* Deserialized log event. */
@decco.decode
type rec event =
  | VarComputation(var_def)
  | FunCall(fun_call)
  | SubScopeCall(sub_scope_call)
@decco.decode
and var_def = {
  pos: option<sourcePosition>,
  name: information,
  io: io_log,
  value: LoggedValue.t,
  fun_calls: option<list<fun_call>>,
}
/* For decco.decode to work, these type declaration have to match *exactly* those
 in catala/runtimes/ocaml/runtime.mli */
@decco.decode
and fun_call = {
  fun_name: information,
  fun_inputs: list<var_def>,
  body: list<event>,
  output: var_def,
}
@decco.decode
and sub_scope_call = {
  @decco.key("name") sname: information,
  inputs: list<var_def>,
  @decco.key("body") sbody: list<event>,
}

let deserializedEvents: array<eventSerialized> => array<event>
