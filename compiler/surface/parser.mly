(*
  This file is part of the Catala compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2020-2025 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>,
  Emile Rolley <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  *)

%{
  open Catala_utils

  let get_pos lpos =
    Pos.overwrite_law_info (Pos.from_lpos lpos)
      (Parser_state.get_current_heading ())

  let type_from_args
      (args :
         (Ast.lident Mark.pos * Ast.base_typ Mark.pos) list Mark.pos option)
      (return_typ : Ast.base_typ Mark.pos) : Ast.typ =
    match args with
    | None -> Mark.map (fun r -> Ast.Base r) return_typ
    | Some (arg_typ, _) ->
      Mark.add (Mark.get return_typ) (Ast.Func { arg_typ; return_typ })

  let move_attrs ~from =
    let moved_attrs, kept_attrs =
      List.partition (function Shared_ast.Src _ -> true | _ -> false)
        (Pos.attrs from)
    in
    let set_attrs target =
      Mark.map_mark (fun pos -> Pos.add_attrs pos moved_attrs) target
    in
    Pos.set_attrs from kept_attrs, set_attrs

  let no_trailing_attrs (x, pending_attrs) =
    (match pending_attrs with
     | Shared_ast.Src ((path, _), _, pos) :: _ ->
       Message.delayed_error () "Unattached trailing atttribute #[%s] ignored"
         ~pos (String.concat "." path)
     | _ -> ());
    x
%}

%parameter<Localisation: sig
  val lex_builtin: string -> Ast.builtin_expression option
end>

(* The token is returned for every line of law text, make them right-associative
   so that we concat them efficiently as much as possible. *)
%right LAW_TEXT

%right top_decl

%right ATTR_START

(* Precedence of expression constructions *)
%right top_expr
%right ALT
%right let_expr
%right AND OR XOR (* Desugaring enforces proper parens later on *)
%nonassoc GREATER GREATER_EQUAL LESSER LESSER_EQUAL EQUAL NOT_EQUAL
%left PLUS MINUS PLUSPLUS
%left MULT DIV
%right apply OF CONTAINS FOR SUCH WITH BUT_REPLACE OR_IF_LIST_EMPTY
%right WITH_V
%right COMMA
%right unop_expr
%right CONTENT
%nonassoc UIDENT
%left DOT

(* Types of all rules, in order. Without this, Menhir type errors are nearly
   impossible to debug because of inlining *)

%type<Pos.t> pos(CONDITION)
%type<Ast.naked_expression Mark.pos> addpos(naked_expression)
%type<Ast.uident Mark.pos> addpos(UIDENT)
%type<Shared_ast.attr_value> attribute_value
%type<Ast.expression> attr(noattr_expression)
%type<Pos.attr> attribute
%type<Ast.base_typ_data Mark.pos> posattr(typ_data)
%type<Ast.primitive_typ> primitive_typ
%type<Ast.base_typ_data> typ_data
%type<Ast.base_typ> typ
%type<Ast.uident Mark.pos> uident
%type<Ast.lident Mark.pos> lident
%type<Ast.scope_var> scope_var
%type<Ast.path * Ast.uident Mark.pos> quident
%type<Ast.path * Ast.lident Mark.pos> qlident
%type<Ast.expression> expression
%type<Ast.naked_expression> naked_expression
%type<Ast.lident Mark.pos * expression> struct_content_field
%type<Ast.naked_expression> struct_or_enum_inject
%type<Ast.literal_number> num_literal
%type<Ast.literal_unit> unit_literal
%type<Ast.literal> literal
%type<(Ast.lident Mark.pos * expression) list> scope_call_args
%type<bool> minmax
%type<Ast.unop> unop
%type<Ast.binop> binop
%type<Ast.match_case_pattern> constructor_binding
%type<Ast.match_case> match_arm
%type<Ast.expression> condition_consequence
%type<Ast.scope_var Mark.pos * Ast.lident Mark.pos list Mark.pos option> rule_expr
%type<bool> rule_consequence
%type<Ast.scope_use_item> rule
%type<Ast.lident Mark.pos list> definition_parameters
%type<Ast.lident Mark.pos> label
%type<Ast.lident Mark.pos> state
%type<Ast.exception_to> exception_to
%type<Ast.scope_use_item> definition
%type<Ast.variation_typ> variation_type
%type<Ast.scope_use_item> assertion
%type<Ast.scope_use_item> scope_item
%type<Ast.lident Mark.pos * Ast.base_typ Mark.pos> struct_scope_base
%type<Ast.struct_decl_field> struct_scope
%type<Ast.io_input option> scope_decl_item_attribute_input
%type<bool> scope_decl_item_attribute_output
%type<Ast.io_input option Mark.pos * bool Mark.pos * Ast.lident Mark.pos> scope_decl_item_attribute
%type<Ast.scope_decl_context_io * Ast.lident Mark.pos> scope_decl_item_attribute_mandatory
%type<Ast.scope_decl_context_item> scope_decl_item
%type<Ast.enum_decl_case> enum_decl_line
%type<Ast.code_item * Pos.attr list> code_item
%type<Ast.code_block> code
%type<Ast.code_block * string Mark.pos> metadata_block
%type<Ast.law_heading> law_heading
%type<string> law_text
%type<Ast.law_structure> source_file_item
%type<Ast.law_structure list> source_file

%start source_file

%%

let pos(x) ==
| x ; { get_pos $loc }

let addpos(x) ==
| a = x ; { (a, get_pos $loc(a)) }

let attribute_value ==
| ~ = addpos(STRING); <Shared_ast.String>
| ~ = expression; <Ast.Expression>

let attribute :=
| ATTR_START ;
  tag = addpos(separated_nonempty_list(DOT,LIDENT)) ;
  value = option(preceded(EQUAL, attribute_value)) ;
  RBRACKET ; {
  Shared_ast.Src (tag, Option.value ~default:Shared_ast.Unit value, get_pos $loc)
}

let attr(x) ==
| a = list(attribute) ; ~ = x ; {
  fst x, Pos.add_attrs (snd x) a
}

let posattr(x) ==
| ~ = attr(addpos(x)) ; <>

let rev_list_with_attr(x) :=
| l = rev_list_with_attr(x) ; a = attribute ; {
  let l, a1 = l in l, a::a1
}
| rest = rev_list_with_attr(x) ; x = addpos(x) ; {
  let prev, attrs = rest in
  (fst x, Pos.add_attrs (snd x) attrs) :: prev, []
}
| { [], [] }

let list_with_attr(x) ==
| l = rev_list_with_attr(x) ; {
  let l, pending_attrs = l in
  List.rev l, List.rev pending_attrs
}

let primitive_typ :=
| INTEGER ; { Integer }
| BOOLEAN ; { Boolean }
| MONEY ; { Money }
| DURATION ; { Duration }
| TEXT ; { Text }
| DECIMAL ; { Decimal }
| DATE ; { Date }
| c = quident ; { let path, uid = c in Named (path, uid) }

let typ_data :=
| t = primitive_typ ; <Primitive>
| LIST ; t = posattr(typ_data) ; <Collection>
| LPAREN ; tl = separated_nonempty_list(COMMA,posattr(typ_data)) ; RPAREN ; {
  match tl with
  | [t, _] -> t
  | ts -> TTuple ts
}

let typ == t = typ_data ; <Data>

let uident ==
| ~ = addpos(UIDENT) ; <>

let lident :=
| i = LIDENT ; {
  match Localisation.lex_builtin i with
  | Some _ ->
     Message.delayed_error
       (i, get_pos $sloc)
       ~kind:Parsing
       ~pos:(get_pos $sloc)
        "Reserved builtin name"
  | None ->
      (i, get_pos $sloc)
}

let scope_var ==
| b = separated_nonempty_list(DOT, addpos(LIDENT)) ; <>

let quident :=
| uid = uident ; DOT ; quid = quident ; {
  let path, quid = quid in uid :: path, quid
}
| id = uident ; { [], id }

let qlident :=
| uid = uident ; DOT ; qlid = qlident ; {
  let path, lid = qlid in uid :: path, lid
}
| id = lident ; { [], id }

let mbinder ==
| id = attr(lident) ; { [id] }
| LPAREN ; ids = separated_nonempty_list(COMMA,attr(lident)) ; RPAREN ; <>

let state_qualifier ==
| STATE ; state = posattr(LIDENT) ; <>

let expression ==
| ~ = attr(noattr_expression) ; <>

let noattr_expression :=
| ~ = addpos(naked_expression) ; <>
(* Required for leading expressions to explicitely apply to the outermost expression *)

let naked_expression ==
| id = addpos(LIDENT) ; state = option(state_qualifier) ; {
  match Localisation.lex_builtin (Mark.remove id), state with
  | Some b, None -> Builtin b
  | Some b, Some _ ->
      Message.delayed_error (Builtin b) ~kind:Parsing ~pos:
        (get_pos $loc(id))
        "Invalid use of built-in @{<bold>%s@}" (Mark.remove id)
  | None, state -> Ident ([], id, state)
}
| uid = uident ; DOT ; qlid = qlident ; {
  let path, lid = qlid in Ident (uid :: path, lid, None)
}
| l = literal ; {
  Literal l
}
| LPAREN ; el = separated_nonempty_list(COMMA, expression) ; RPAREN ; {
  match el with
  | [e] -> Paren e
  | es -> Tuple es
}
| e = noattr_expression ;
  DOT ; i = addpos(qlident) ; <Dotted>
| e = noattr_expression ; DOT ; arg = addpos(INT_LITERAL) ; {
  let n_str, pos_n = arg in
  let n = int_of_string n_str in
  if n <= 0 then
    Message.delayed_error ()
      ~kind:Parsing
      ~pos:pos_n "Tuple indices must be >= 1";
  TupleAccess (e, (n, pos_n))
}
| CARDINAL ; {
  Builtin Cardinal
}
| INTEGER ; {
  Builtin ToInteger
}
| DECIMAL ; {
  Builtin ToDecimal
}
| MONEY ; {
  Builtin ToMoney
}
| LBRACKET ; l = separated_list(SEMICOLON, expression) ; RBRACKET ;
  <ArrayLit>
| e = struct_or_enum_inject ; <>
| e1 = noattr_expression ;
  OF ;
  args = funcall_args ; {
  FunCall (e1, args)
}
| OUTPUT ; OF ;
  c = addpos(quident) ;
  fields = scope_call_args ; {
  ScopeCall (c, fields)
}
| e = noattr_expression ;
  WITH ; c = constructor_binding ; {
  TestMatchCase (e, (c, get_pos $sloc))
}
| e = noattr_expression ;
  BUT_REPLACE ;
  LBRACE ;
  fields = nonempty_list(preceded (ALT, struct_content_field)) ;
  RBRACE ; {
  StructReplace (e, fields)
}
| coll = noattr_expression ;
  pos = pos(CONTAINS) ;
  element = expression ; {
  CollectionOp ((Member { element }, pos), coll)
} %prec apply
| pos = pos(SUM) ; typ = addpos(primitive_typ) ;
  OF ; coll = expression ; {
  CollectionOp ((AggregateSum { typ = Mark.remove typ }, pos), coll)
} %prec apply
| f = noattr_expression ;
  pos = pos(FOR) ; i = mbinder ;
  AMONG ; coll = expression ; {
  CollectionOp ((Map {f = i, f}, pos), coll)
} %prec apply
| pos = pos(COMBINE) ; acc = mbinder ; INITIALLY ; init = expression ;
  WITH_V ; map_expr = expression ; {
  match map_expr with
  | CollectionOp ((Map { f = i, f }, _), coll), _ ->
    CollectionOp ((Fold {f = acc, i, f; init = init}, pos), coll)
  | _ ->
     let init, _ = init in
     Message.delayed_error
       init (* dummy value *)
       ~kind:Parsing ~pos:(snd map_expr)
      "Expected the form '<expr> for <var> among <collection>'"
} %prec apply
| maxp = addpos(minmax) ;
  OF ; coll = expression ; default = opt_or_if_empty ; {
  let max, pos = maxp in
  CollectionOp ((AggregateExtremum { max; default }, pos), coll)
}
| op = addpos(unop) ; e = expression ; {
  Unop (op, e)
} %prec unop_expr
| e1 = noattr_expression ;
  binop = addpos(binop) ;
  e2 = expression ; {
  Binop (binop, e1, e2)
}
| pos = pos(EXISTS) ; i = mbinder ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; predicate = expression ; {
  CollectionOp ((Exists {predicate = i, predicate}, pos), coll)
} %prec let_expr
| pos = pos(FOR) ; ALL ; i = mbinder ;
  AMONG ; coll = expression ;
  WE_HAVE ; predicate = expression ; {
  CollectionOp ((Forall {predicate = i, predicate}, pos), coll)
} %prec let_expr
| MATCH ; e = expression ;
  WITH ;
  arms = addpos(nonempty_list(addpos(preceded(ALT, match_arm)))) ; {
  MatchWith (e, arms)
}
| IF ; e1 = expression ;
  THEN ; e2 = expression ;
  ELSE ; e3 = expression ; {
  IfThenElse (e1, e2, e3)
} %prec let_expr
| LET ; ids = mbinder ;
  DEFINED_AS ; e1 = expression ;
  IN ; e2 = expression ; {
  LetIn (ids, e1, e2)
} %prec let_expr
| pos = pos(LIST); ids = mbinder ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; f = expression ; {
  CollectionOp ((Filter {f = ids, f}, pos), coll)
} %prec top_expr
| fmap = noattr_expression ;
  pfor = pos(FOR) ; i = mbinder ;
  AMONG ; coll = expression ;
  psuch = pos(SUCH) ; THAT ; ffilt = expression ; {
  CollectionOp ((Map {f = i, fmap}, pfor), (CollectionOp ((Filter {f = i, ffilt}, psuch), coll), get_pos $loc))
} %prec top_expr
| pos = pos(CONTENT); OF; ids = mbinder ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; f = expression ;
  IS ; max = minmax ;
  default = opt_or_if_empty; {
  CollectionOp ((AggregateArgExtremum { max; default; f = ids, f }, pos), coll)
}

let opt_or_if_empty ==
| OR_IF_LIST_EMPTY ; THEN ; default = expression ; <Some> %prec apply
| { None } %prec apply

let struct_content_field :=
| field = lident ; COLON ; e = expression ; <>

let struct_or_enum_inject ==
| uid = addpos(quident) ;
  data = option(preceded(CONTENT,expression)) ; {
  EnumInject(uid, data)
}
| c = addpos(quident) ;
  LBRACE ;
  fields = nonempty_list(preceded(ALT, struct_content_field)) ;
  RBRACE ; {
  StructLit(c, fields)
}

let num_literal ==
| d = INT_LITERAL ; <Int>
| d = DECIMAL_LITERAL ; {
  let (d1, d2) = d in Dec (d1, d2)
}

let unit_literal ==
| PERCENT ; { Percent }
| YEAR ; { Year}
| MONTH ; { Month }
| DAY ; { Day }

let literal :=
| l = addpos(num_literal); u = option(addpos(unit_literal)) ; <LNumber>
| money = MONEY_AMOUNT ; {
  let (units, cents) = money in
  LMoneyAmount {
    money_amount_units = units;
    money_amount_cents = cents;
  }
}
| d = DATE_LITERAL ; {
  let (y,m,d) = d in
  LDate {
    literal_date_year = y;
    literal_date_month = m;
    literal_date_day = d;
  }
}
| TRUE ; { LBool true }
| FALSE ; { LBool false }

let scope_call_args ==
| { [] }
| WITH_V ;
  LBRACE ;
  fields = list(preceded (ALT, struct_content_field)) ;
  RBRACE ; {
  fields
}

let funcall_args :=
| e = expression; { [e] } %prec apply
| e = expression; COMMA; el = funcall_args ; { e :: el }

let minmax ==
| MAXIMUM ; { true }
| MINIMUM ; { false }

let unop ==
| NOT ; { Not }
| k = MINUS ; <Minus>

let binop ==
| k = MULT ; <Mult>
| k = DIV ; <Div>
| k = PLUS ; <Add>
| k = MINUS ; <Sub>
| PLUSPLUS ; { Concat }
| k = LESSER ; <Lt>
| k = LESSER_EQUAL ; <Lte>
| k = GREATER ; <Gt>
| k = GREATER_EQUAL ; <Gte>
| EQUAL ; { Eq }
| NOT_EQUAL ; { Neq }
| AND ; { And }
| OR ; { Or }
| XOR ; { Xor }

let constructor_binding :=
| uid = addpos(quident) ; OF ; lid = lident ; {
  ([uid], Some lid)
}
| uid = addpos(quident) ; {
  ([uid], None)
} %prec apply

let match_arm :=
| WILDCARD ; COLON ; ~ = expression ; <WildCard>
  %prec ALT
| pat = addpos(constructor_binding) ;
  COLON ; e = expression ; {
  MatchCase {
    match_case_pattern = pat;
    match_case_expr = e;
  }
} %prec ALT

let condition_consequence :=
| UNDER_CONDITION ; c = expression ; CONSEQUENCE ; <>

let rule_expr ==
| i = posattr(scope_var) ; p = option(posattr(definition_parameters)) ; <>

let rule_consequence :=
| flag = option(NOT); FILLED ; {
  None = flag
}

let rule :=
| label = ioption(label) ;
  except = ioption(exception_to) ;
  _rule = RULE ;
  name_and_param = rule_expr ;
  state = option(state) ;
  cond = option(condition_consequence) ;
  consequence = addpos(rule_consequence) ; {
  let (name, params_applied) = name_and_param in
  let cons : bool Mark.pos = consequence in
  let rule_exception = match except with
    | None -> NotAnException
    | Some x -> x
  in
  let pos_start =
    match label with Some _ -> get_pos $loc(label)
    | None -> match except with Some _ -> get_pos $loc(except)
    | None -> get_pos $loc(_rule)
  in
  Rule {
    rule_label = label;
    rule_exception_to = rule_exception;
    rule_parameter = params_applied;
    rule_condition = cond;
    rule_name = name;
    rule_id = Shared_ast.RuleName.fresh
      (String.concat "." (List.map (fun i -> Mark.remove i) (Mark.remove name)),
       Pos.join pos_start (Mark.get name));
    rule_consequence = cons;
    rule_state = state;
  }
}

let definition_parameters :=
| OF ; args = separated_nonempty_list(COMMA,lident) ; <>


let label ==
| LABEL ; i = lident ; <>

let state :=
| STATE ; s = lident ; <>

let exception_to ==
| EXCEPTION ; i = ioption(lident) ; {
  match i with
  | None -> UnlabeledException
  | Some x -> ExceptionToLabel x
}

let definition :=
| label = ioption(label) ;
  except = ioption(exception_to) ;
  _def = DEFINITION ;
  name = scope_var ;
  params = option(addpos(definition_parameters)) ;
  state = option(state) ;
  cond = option(condition_consequence) ;
  DEFINED_AS ;
  e = expression ; {
  let def_exception = match except with
    | None -> NotAnException
    | Some x -> x
  in
  let name_pos = $loc(name) in
  let start_pos = match label with
    | Some _ -> fst $loc(label)
    | None -> match except with
      | Some _ -> fst $loc(except)
      | None -> fst $loc(_def)
  in
  let pos = get_pos (start_pos, snd $loc(name)) in
  Definition {
    definition_label = label;
    definition_exception_to = def_exception;
    definition_name = name, get_pos name_pos;
    definition_parameter = params;
    definition_condition = cond;
    definition_id =
      Shared_ast.RuleName.fresh
        (String.concat "." (List.map (fun i -> Mark.remove i) name),
         pos);
    definition_expr = e;
    definition_state = state;
  }
}

let assertion :=
| ASSERTION ;
  cond = option(condition_consequence) ;
  base = expression ; {
  Assertion {
    assertion_condition = cond;
    assertion_content = base;
  }
}

let variation_type :=
| INCREASING ; { Increasing }
| DECREASING ; { Decreasing }

let date_rounding :=
| DATE ; i = LIDENT ; v = addpos(variation_type); {
  (* Round is a builtin, we need to check which one it is *)
  match Localisation.lex_builtin i with
  | Some Round ->
    DateRounding v
  | _ ->
    Message.delayed_error
      (DateRounding v)
      ~kind:Parsing ~pos:(get_pos $loc(i))
      "Expected the form 'date round increasing' or 'date round decreasing'"
}

let scope_item ==
| ~ = rule ; <>
| ~ = definition ; <>
| ~ = assertion ; <>
| ~ = date_rounding ; <>

let scope_item_list ==
| items = list_with_attr(scope_item) ; {
  let items, pending_attrs = items in
  List.map (fun (item, apos) ->
    let apos_noattr, set_attrs = move_attrs ~from:apos in
    match item with
    | Rule r ->
      Rule {r with rule_id =
            Shared_ast.RuleName.map_info set_attrs r.rule_id},
      apos_noattr
    | Definition d ->
      Definition { d with definition_id =
                   Shared_ast.RuleName.map_info set_attrs d.definition_id },
      apos_noattr
    | Assertion a -> Assertion a, apos
    | DateRounding r -> DateRounding r, apos
  ) items,
  pending_attrs
} %prec ATTR_START

let struct_scope_base :=
| DATA ; i = lident ;
  CONTENT ; t = posattr(typ) ; <>
| pos = pos(CONDITION) ; i = lident ; {
  (i, (Condition, pos))
}

let struct_scope :=
| name_and_typ = struct_scope_base ;
  args = depends_stance; {
  let (name, typ) = name_and_typ in
  {
    struct_decl_field_name = name;
    struct_decl_field_typ = type_from_args args typ;
  }
}

let scope_decl_item_attribute_input ==
| CONTEXT ; { Some Context }
| INPUT ; { Some Input }
| INTERNAL ; { Some Internal }
| { None }

let scope_decl_item_attribute_output ==
| OUTPUT ; { true }
| { false }

let scope_decl_item_attribute ==
| input = addpos(scope_decl_item_attribute_input) ;
  output = addpos(scope_decl_item_attribute_output) ;
  i = lident ; {
  match input, output with
  | (Some Internal, _), (true, pos) ->
     Message.delayed_error
       (input, output, i)
       ~kind:Parsing ~pos
      "A variable cannot be declared both 'internal' and 'output'."
  | input, output -> input, output, i
}

let scope_decl_item_attribute_mandatory ==
| iattr = scope_decl_item_attribute ; {
  let in_attr_opt, out_attr, i = iattr in
  let in_attr = match in_attr_opt, out_attr with
    | (None, _), (false, _) ->
       Message.delayed_error
         (Internal, get_pos $loc(iattr))
         ~kind:Parsing ~pos:(get_pos $loc(iattr))
         "Variable declaration requires input qualification \
          ('internal', 'input' or 'context')"
    | (None, pos), (true, _) -> Internal, pos
    | (Some i, pos), _ -> i, pos
  in
  {
    scope_decl_context_io_input = in_attr;
    scope_decl_context_io_output = out_attr;
  }, i
}

let scope_decl_item :=
| attr_i = scope_decl_item_attribute_mandatory ;
  CONTENT ; t = addpos(typ) ;
  args_typ = depends_stance ;
  states = list(state) ; {
  let attr, i = attr_i in
  ContextData {
  scope_decl_context_item_name = i;
  scope_decl_context_item_attribute = attr;
  scope_decl_context_item_parameters =
    Option.map
      (Mark.map
         (List.map (fun (lbl, (base_t, m)) -> lbl, (Base base_t, m))))
      args_typ;
  scope_decl_context_item_typ = type_from_args args_typ t;
  scope_decl_context_item_states = states;
  }
}
| attr = scope_decl_item_attribute ;
  SCOPE ; c = addpos(quident) ; {
  let in_attr_opt, out_attr, i = attr in
  let attr = match in_attr_opt, out_attr with
    | (None, pos), out -> {
        scope_decl_context_io_input = (Internal, pos);
        scope_decl_context_io_output = out;
      };
    | (Some _, pos), out ->
       Message.delayed_error
         {
           scope_decl_context_io_input = (Internal, pos);
           scope_decl_context_io_output = out;
         }
         ~kind:Parsing ~pos
          "Scope declaration does not support input qualifiers ('internal', \
           'input' or 'context')"
  in
  ContextScope{
    scope_decl_context_scope_name = i;
    scope_decl_context_scope_sub_scope = c;
    scope_decl_context_scope_attribute = attr;
  }
}
| attr_i = scope_decl_item_attribute_mandatory ;
  pos_condition = pos(CONDITION) ;
  args = depends_stance ;
  states = list(state) ; {
  let attr, i = attr_i in
  ContextData {
    scope_decl_context_item_name = i;
    scope_decl_context_item_attribute = attr;
    scope_decl_context_item_parameters =
      Option.map
        (Mark.map
           (List.map (fun (lbl, (base_t, m)) -> lbl, (Base base_t, m))))
        args;
    scope_decl_context_item_typ =
      type_from_args args (Condition, pos_condition);
    scope_decl_context_item_states = states;
  }
}

let enum_decl_line :=
| ALT ; c = uident ;
  t = option(preceded(CONTENT,posattr(typ))) ; {
  {
    enum_decl_case_name = c;
    enum_decl_case_typ =
      Option.map (fun (t, t_pos) ->  Base t, t_pos) t;
  }
}

let var_content ==
| ~ = lident ; CONTENT ; ty = posattr(typ) ; <>
let depends_stance ==
| DEPENDS ; args = separated_nonempty_list(COMMA,var_content) ; {
  Some (args, get_pos $sloc)
}
| DEPENDS ; LPAREN ; args = separated_nonempty_list(COMMA,var_content) ; RPAREN ; {
  Some (args, get_pos $sloc)
}
| { None }

let code_item :=
| SCOPE ; c = uident ;
  e = option(preceded(UNDER_CONDITION,expression)) ;
  COLON ; items = scope_item_list ; {
  let items, pending_attrs = items in
  ScopeUse {
    scope_use_name = c;
    scope_use_condition = e;
    scope_use_items = items;
  }, pending_attrs
}
| DECLARATION ; STRUCT ; c = attr(uident) ;
  COLON ; scopes = list_with_attr(struct_scope) ; {
  let scopes, pending_attrs = scopes in
  StructDecl {
    struct_decl_name = c;
    struct_decl_fields =
      List.map (fun (field, pos) ->
          let pos_noattr, set_attrs = move_attrs ~from:pos in
          { field with struct_decl_field_name = set_attrs field.struct_decl_field_name },
          pos_noattr)
        scopes;
  }, pending_attrs
} %prec top_decl
| DECLARATION ; SCOPE ; c = uident ;
  COLON ; context = list_with_attr(scope_decl_item) ; {
  let context, pending_attrs = context in
  ScopeDecl {
    scope_decl_name = c;
    scope_decl_context =
      List.map (fun (ctx, pos) ->
          let pos_noattr, set_attrs = move_attrs ~from:pos in
          (match ctx with
           | ContextData d ->
               ContextData { d with scope_decl_context_item_name =
                             set_attrs d.scope_decl_context_item_name }
           | ContextScope s ->
               ContextScope { s with scope_decl_context_scope_name =
                             set_attrs s.scope_decl_context_scope_name }),
          pos_noattr)
        context;
  }, pending_attrs
} %prec top_decl
| DECLARATION ; ENUM ; c = uident ;
  COLON ; cases = list_with_attr(enum_decl_line) ; {
  let cases, pending_attrs = cases in
  EnumDecl {
    enum_decl_name = c;
    enum_decl_cases =
      List.map (fun (case, pos) ->
          let pos_noattr, set_attrs = move_attrs ~from:pos in
          { case with enum_decl_case_name = set_attrs case.enum_decl_case_name },
          pos_noattr)
        cases;
  }, pending_attrs
} %prec top_decl
| DECLARATION ; name = lident ;
  CONTENT ; ty = addpos(typ) ;
  args = depends_stance ;
  topdef_expr = option(opt_def) ; {
  Topdef {
    topdef_name = name;
    topdef_args = args;
    topdef_type = type_from_args args ty;
    topdef_expr;
  }, []
}

let opt_def ==
| DEFINED_AS; e = expression; <>

let code ==
| items = list_with_attr(code_item) ; {
  let items = no_trailing_attrs items in
  List.rev @@ no_trailing_attrs @@
  List.fold_left (fun (acc, pending_attrs) ((item, trailing_attrs), pos) ->
    let pos = Pos.add_attrs pos pending_attrs in
    let pos_noattr, set_attrs = move_attrs ~from:pos in
    let item =
      match item with
      | ScopeUse su -> ScopeUse { su with scope_use_name = set_attrs su.scope_use_name }
      | StructDecl sd -> StructDecl { sd with struct_decl_name = set_attrs sd.struct_decl_name }
      | ScopeDecl sd -> ScopeDecl { sd with scope_decl_name = set_attrs sd.scope_decl_name }
      | EnumDecl ed -> EnumDecl { ed with enum_decl_name = set_attrs ed.enum_decl_name }
      | Topdef td -> Topdef { td with topdef_name = set_attrs td.topdef_name }
    in
    (item, pos_noattr) :: acc, trailing_attrs
  ) ([], []) items
}

let metadata_block :=
| BEGIN_METADATA ; option(law_text) ;
  ~ = code ;
  text = END_CODE ; {
  (code, (text, get_pos $sloc))
}

let law_heading :=
| heading = LAW_HEADING ; {
  Parser_state.new_heading heading $sloc
}

let law_text :=
| lines = nonempty_list(LAW_TEXT) ; { String.trim (String.concat "" lines) }

let directive :=
| LAW_INCLUDE ; COLON ;
  args = nonempty_list(DIRECTIVE_ARG) ;
  page = option(AT_PAGE) ; {
  let filename = String.trim (String.concat "" args) in
  let pos = get_pos $sloc in
  let jorftext = Re.Pcre.regexp "(JORFARTI\\d{12}|LEGIARTI\\d{12}|CETATEXT\\d{12})" in
  if Re.Pcre.pmatch ~rex:jorftext filename && page = None then
    LawInclude (Ast.LegislativeText (filename, pos))
  else if Filename.extension filename = ".pdf" || page <> None then
    LawInclude (Ast.PdfFile ((filename, pos), page))
  else
    LawInclude (Ast.CatalaFile (filename, pos))
}
| MODULE_DEF ; m = addpos(DIRECTIVE_ARG) ;
  ext = option (MODULE_EXTERNAL) ; {
  ModuleDef (m, ext <> None)
}
| MODULE_USE ; m = addpos(DIRECTIVE_ARG) ;
  alias = option (preceded(MODULE_ALIAS,addpos(DIRECTIVE_ARG))) ; {
  ModuleUse (m, alias)
}

let source_file_item :=
| text = law_text ; { LawText text }
| BEGIN_CODE ;
  ~ = code ;
  text = END_CODE ; {
  CodeBlock (code, (text, get_pos $sloc), false)
}
| heading = law_heading ; {
  LawHeading (heading, [])
}
| code = metadata_block ; {
  let (code, source_repr) = code in
  CodeBlock (code, source_repr, true)
}
| BEGIN_DIRECTIVE ; ~ = directive ; END_DIRECTIVE ; { directive }

let source_file :=
| hd = source_file_item ; tl = source_file ; { hd::tl }
| EOF ; { [] }
