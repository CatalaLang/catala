(*
  This file is part of the Catala compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>,
  Emile Rolley <emile.rolley@tuta.io>

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
%}

%parameter<Localisation: sig
  val lex_builtin: string -> Ast.builtin_expression option
end>

(* The token is returned for every line of law text, make them right-associative
   so that we concat them efficiently as much as possible. *)
%right LAW_TEXT

(* Precedence of expression constructions *)
%right top_expr
%right ALT
%right let_expr IS
%right AND OR XOR (* Desugaring enforces proper parens later on *)
%nonassoc GREATER GREATER_EQUAL LESSER LESSER_EQUAL EQUAL NOT_EQUAL
%left PLUS MINUS PLUSPLUS
%left MULT DIV
%right apply OF CONTAINS FOR SUCH WITH
%right unop_expr
%right CONTENT
%nonassoc UIDENT
%left DOT

(* Types of all rules, in order. Without this, Menhir type errors are nearly
   impossible to debug because of inlining *)

%type<Ast.uident Marked.pos> addpos(UIDENT)
%type<Pos.t> pos(CONDITION)
%type<Ast.primitive_typ> typ_base
%type<Ast.base_typ_data> typ
%type<Ast.uident Marked.pos> uident
%type<Ast.lident Marked.pos> lident
%type<Ast.scope_var> scope_var
%type<Ast.path * Ast.uident Marked.pos> quident
%type<Ast.path * Ast.lident Marked.pos> qlident
%type<Ast.expression> expression
%type<Ast.naked_expression> naked_expression
%type<Ast.lident Marked.pos * expression> struct_content_field
%type<Ast.naked_expression> struct_or_enum_inject
%type<Ast.literal_number> num_literal
%type<Ast.literal_unit> unit_literal
%type<Ast.literal> literal
%type<(Ast.lident Marked.pos * expression) list> scope_call_args
%type<bool> minmax
%type<Ast.unop> unop
%type<Ast.binop> binop
%type<Ast.match_case_pattern> constructor_binding
%type<Ast.match_case> match_arm
%type<Ast.expression> condition_consequence
%type<Ast.scope_var Marked.pos * Ast.lident Marked.pos option> rule_expr
%type<bool> rule_consequence
%type<Ast.rule> rule
%type<Ast.lident Marked.pos> definition_parameters
%type<Ast.lident Marked.pos> label
%type<Ast.lident Marked.pos> state
%type<Ast.exception_to> exception_to
%type<Ast.definition> definition
%type<Ast.variation_typ> variation_type
%type<Ast.scope_use_item> assertion
%type<Ast.scope_use_item> scope_item
%type<Ast.lident Marked.pos * Ast.base_typ Marked.pos> struct_scope_base
%type<Ast.base_typ_data Marked.pos> struct_scope_func
%type<Ast.struct_decl_field> struct_scope
%type<Ast.io_input> scope_decl_item_attribute_input
%type<bool> scope_decl_item_attribute_output
%type<Ast.scope_decl_context_io> scope_decl_item_attribute
%type<Ast.scope_decl_context_item> scope_decl_item
%type<Ast.enum_decl_case> enum_decl_line
%type<Ast.code_item> code_item
%type<Ast.code_block> code
%type<Ast.code_block * string Marked.pos> metadata_block
%type<Ast.law_heading> law_heading
%type<string> law_text
%type<Ast.law_structure> source_file_item
%type<Ast.law_structure list> source_file

%start source_file

%%

let pos(x) ==
| x ; { Pos.from_lpos $loc }

let addpos(x) ==
| ~=x ; { x, Pos.from_lpos $loc(x) }

let typ_base :=
| INTEGER ; { Integer }
| BOOLEAN ; { Boolean }
| MONEY ; { Money }
| DURATION ; { Duration }
| TEXT ; { Text }
| DECIMAL ; { Decimal }
| DATE ; { Date }
| c = quident ; { let path, uid = c in Named (path, uid) }

let typ :=
| t = typ_base ; <Primitive>
| COLLECTION ; t = addpos(typ) ; <Collection>

let uident ==
| ~ = addpos(UIDENT) ; <>

let lident :=
| i = LIDENT ; {
  match Localisation.lex_builtin i with
  | Some _ ->
      Errors.raise_spanned_error
        (Pos.from_lpos $sloc)
        "Reserved builtin name"
  | None ->
      (i, Pos.from_lpos $sloc)
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

let expression :=
| e = addpos(naked_expression) ; <>

let naked_expression ==
| id = addpos(LIDENT) ; {
  match Localisation.lex_builtin (Marked.unmark id) with
  | Some b -> Builtin b
  | None -> Ident ([], id)
}
| uid = uident ; DOT ; qlid = qlident ; {
  let path, lid = qlid in Ident (uid :: path, lid)
}
| l = literal ; {
  Literal l
}
| LPAREN ; e = expression ; RPAREN ; <Paren>
| e = expression ;
  DOT ; i = addpos(qlident) ; <Dotted>
| CARDINAL ; {
  Builtin Cardinal
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
| e1 = expression ;
  OF ;
  e2 = expression ; {
  FunCall (e1, e2)
} %prec apply
| OUTPUT ; OF ;
  c = addpos(quident) ;
  fields = option(scope_call_args) ; {
  let fields = Option.value ~default:[] fields in
  ScopeCall (c, fields)
}
| e = expression ;
  WITH ; c = constructor_binding ; {
  TestMatchCase (e, (c, Pos.from_lpos $sloc))
}
| e1 = expression ;
  CONTAINS ;
  e2 = expression ; {
  MemCollection (e2, e1)
} %prec apply
| SUM ; typ = addpos(typ_base) ;
  OF ; coll = expression ; {
  CollectionOp (AggregateSum { typ = Marked.unmark typ }, coll)
} %prec apply
| f = expression ;
  FOR ; i = lident ;
  AMONG ; coll = expression ; {
  CollectionOp (Map {f = i, f}, coll)
} %prec apply
| max = minmax ;
  OF ; coll = expression ;
  OR ; IF ; COLLECTION ; EMPTY ; THEN ;
  default = expression ; {
  CollectionOp (AggregateExtremum { max; default }, coll)
} %prec apply
| op = addpos(unop) ; e = expression ; {
  Unop (op, e)
} %prec unop_expr
| e1 = expression ;
  binop = addpos(binop) ;
  e2 = expression ; {
  Binop (binop, e1, e2)
}
| EXISTS ; i = lident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; predicate = expression ; {
  CollectionOp (Exists {predicate = i, predicate}, coll)
} %prec let_expr
| FOR ; ALL ; i = lident ;
  AMONG ; coll = expression ;
  WE_HAVE ; predicate = expression ; {
  CollectionOp (Forall {predicate = i, predicate}, coll)
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
| LET ; id = lident ;
  DEFINED_AS ; e1 = expression ;
  IN ; e2 = expression ; {
  LetIn (id, e1, e2)
} %prec let_expr
| i = lident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; f = expression ; {
  CollectionOp (Filter {f = i, f}, coll)
} %prec top_expr
| fmap = expression ;
  FOR ; i = lident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; ffilt = expression ; {
  CollectionOp (Map {f = i, fmap}, (CollectionOp (Filter {f = i, ffilt}, coll), Pos.from_lpos $loc))
} %prec top_expr
| i = lident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; f = expression ;
  IS ; max = minmax ;
  OR ; IF ; COLLECTION ; EMPTY ; THEN ; default = expression ; {
  CollectionOp (AggregateArgExtremum { max; default; f = i, f }, coll)
} %prec top_expr


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
| BAR ; d = DATE_LITERAL ; BAR ; {
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
| WITH_V ;
  LBRACE ;
  fields = list(preceded (ALT, struct_content_field)) ;
  RBRACE ; {
  fields
}

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

let rule_expr :=
| i = addpos(scope_var) ; p = option(definition_parameters) ; <>

let rule_consequence :=
| flag = option(NOT); FILLED ; {
  None = flag
}

let rule :=
| label = option(label) ;
  except = option(exception_to) ;
  RULE ;
  name_and_param = rule_expr ;
  cond = option(condition_consequence) ;
  state = option(state) ;
  consequence = addpos(rule_consequence) ; {
  let (name, param_applied) = name_and_param in
  let cons : bool Marked.pos = consequence in
  let rule_exception = match except with
    | None -> NotAnException
    | Some x -> x
  in
  {
    rule_label = label;
    rule_exception_to = rule_exception;
    rule_parameter = param_applied;
    rule_condition = cond;
    rule_name = name;
    rule_id = Shared_ast.RuleName.fresh
      (String.concat "." (List.map (fun i -> Marked.unmark i) (Marked.unmark name)),
       Pos.from_lpos $sloc);
    rule_consequence = cons;
    rule_state = state;
  }
}

let definition_parameters :=
| OF ; i = lident ; <>

let label :=
| LABEL ; i = lident ; <>

let state :=
| STATE ; s = lident ; <>

let exception_to :=
| EXCEPTION ; i = option(lident) ; {
  match i with
  | None -> UnlabeledException
  | Some x -> ExceptionToLabel x
}

let definition :=
| label = option(label);
  except = option(exception_to) ;
  DEFINITION ;
  name = addpos(scope_var) ;
  param = option(definition_parameters) ;
  state = option(state) ;
  cond = option(condition_consequence) ;
  DEFINED_AS ;
  e = expression ; {
  let def_exception = match except with
    | None -> NotAnException
    | Some x -> x
  in
  {
    definition_label = label;
    definition_exception_to = def_exception;
    definition_name = name;
    definition_parameter = param;
    definition_condition = cond;
    definition_id =
      Shared_ast.RuleName.fresh
        (String.concat "." (List.map (fun i -> Marked.unmark i) (Marked.unmark name)),
          Pos.from_lpos $sloc);
    definition_expr = e;
    definition_state = state;
  }
}

let variation_type :=
| INCREASING ; { Increasing }
| DECREASING ; { Decreasing }

let assertion :=
| cond = option(condition_consequence) ;
  base = expression ; {
  (Assertion {
    assertion_condition = cond;
    assertion_content = base;
  })
}
| FIXED ; q = addpos(scope_var) ; BY ; i = lident ; {
  MetaAssertion (FixedBy (q, i))
}
| VARIES ; q = addpos(scope_var) ;
  WITH_V ; e = expression ;
  t = option(addpos(variation_type)) ; {
  MetaAssertion (VariesWith (q, e, t))
}

let scope_item :=
| r = rule ; <Rule>
| d = definition ; <Definition>
| ASSERTION ; contents = assertion ; <>

let struct_scope_base :=
| DATA ; i = lident ;
  CONTENT ; t = addpos(typ) ; {
  let t, pos = t in
  (i, (Data t, pos))
}
| pos = pos(CONDITION) ; i = lident ; {
  (i, (Condition, pos))
}

let struct_scope_func ==
| DEPENDS ; t = addpos(typ) ; <>

let struct_scope :=
| name_and_typ = struct_scope_base ;
  func_typ = option(struct_scope_func) ; {
  let (name, typ) = name_and_typ in
  let (typ, typ_pos) = typ in
  {
    struct_decl_field_name = name;
    struct_decl_field_typ = match func_typ with
    | None -> (Base typ, typ_pos)
    | Some (arg_typ, arg_pos) ->
      Func {
        arg_typ = (Data arg_typ, arg_pos);
        return_typ = (typ, typ_pos);
      }, Pos.from_lpos $sloc ;
  }
}

let scope_decl_item_attribute_input :=
| CONTEXT ; { Context }
| INPUT ; { Input }

let scope_decl_item_attribute_output :=
| OUTPUT ; { true }
| { false }

let scope_decl_item_attribute :=
| input = addpos(scope_decl_item_attribute_input) ;
  output = addpos(scope_decl_item_attribute_output) ; {
    {
      scope_decl_context_io_input = input;
      scope_decl_context_io_output = output
    }
  }
| INTERNAL ; {
    {
      scope_decl_context_io_input = (Internal, Pos.from_lpos $sloc);
      scope_decl_context_io_output = (false, Pos.from_lpos $sloc)
    }
  }
| OUTPUT ; {
    {
      scope_decl_context_io_input = (Internal, Pos.from_lpos $sloc);
      scope_decl_context_io_output = (true, Pos.from_lpos $sloc)
    }
  }


let scope_decl_item :=
| attr = scope_decl_item_attribute ;
  i = lident ;
  CONTENT ; t = addpos(typ) ;
  func_typ = option(struct_scope_func) ;
  states = list(state) ; {
  ContextData {
  scope_decl_context_item_name = i;
  scope_decl_context_item_attribute = attr;
  scope_decl_context_item_typ =
    (let (typ, typ_pos) = t in
    match func_typ with
    | None -> (Base (Data typ), typ_pos)
    | Some (arg_typ, arg_pos) ->
      Func {
        arg_typ = (Data arg_typ, arg_pos);
        return_typ = (Data typ, typ_pos);
      }, Pos.from_lpos $sloc);
  scope_decl_context_item_states = states;
  }
}
| i = lident ; SCOPE ; c = uident ; {
  ContextScope{
    scope_decl_context_scope_name = i;
    scope_decl_context_scope_sub_scope = c;
    scope_decl_context_scope_attribute = {
      scope_decl_context_io_input = (Internal, Pos.from_lpos $sloc);
      scope_decl_context_io_output = (false, Pos.from_lpos $sloc);
    };
  }
}
| attr = scope_decl_item_attribute ;
  i = lident ;
  pos_condition = pos(CONDITION) ;
  func_typ = option(struct_scope_func) ;
  states = list(state) ; {
  ContextData {
    scope_decl_context_item_name = i;
    scope_decl_context_item_attribute = attr;
    scope_decl_context_item_typ =
      (match func_typ with
      | None -> (Base (Condition), pos_condition)
      | Some (arg_typ, arg_pos) ->
        Func {
          arg_typ = (Data arg_typ, arg_pos);
          return_typ = (Condition, pos_condition);
        }, Pos.from_lpos $sloc);
    scope_decl_context_item_states = states;
  }
}

let enum_decl_line :=
| ALT ; c = uident ;
  t = option(preceded(CONTENT,addpos(typ))) ; {
  {
    enum_decl_case_name = c;
    enum_decl_case_typ =
      Option.map (fun (t, t_pos) ->  Base (Data t), t_pos) t;
  }
}

(* let def_depends ==
 * | DEPENDS ; arg = lident ; CONTENT ; ty_arg = addpos(typ) ; <> *)

let var_content ==
| ~ = lident ; CONTENT ; ty = addpos(typ) ; <>
let depends_stance ==
| DEPENDS ; args = separated_nonempty_list(COMMA,var_content) ; <>
| DEPENDS ; LPAREN ; args = separated_nonempty_list(COMMA,var_content) ; RPAREN ; <>

let code_item :=
| SCOPE ; c = uident ;
  e = option(preceded(UNDER_CONDITION,expression)) ;
  COLON ; items = nonempty_list(addpos(scope_item)) ; {
  ScopeUse {
    scope_use_name = c;
    scope_use_condition = e;
    scope_use_items = items;
  }
}
| DECLARATION ; STRUCT ; c = uident ;
  COLON ; scopes = list(addpos(struct_scope)) ; {
  StructDecl {
    struct_decl_name = c;
    struct_decl_fields = scopes;
  }
}
| DECLARATION ; SCOPE ; c = uident ;
  COLON ; context = nonempty_list(addpos(scope_decl_item)) ; {
  ScopeDecl {
    scope_decl_name = c;
    scope_decl_context = context;
  }
}
| DECLARATION ; ENUM ; c = uident ;
  COLON ; cases = list(addpos(enum_decl_line)) ; {
  EnumDecl {
    enum_decl_name = c;
    enum_decl_cases = cases;
  }
}
| DECLARATION ; name = lident ;
  CONTENT ; ty = addpos(typ) ;
  args = depends_stance ;
  DEFINED_AS ; e = expression ; {
  TopDef {
    topdef_name = name;
    topdef_args = args;
    topdef_type = ty;
    topdef_expr = e;
  }
}

let code :=
| code = list(addpos(code_item)) ; <>

let metadata_block :=
| BEGIN_METADATA ; option(law_text) ;
  ~ = code ;
  text = END_CODE ; {
  (code, (text, Pos.from_lpos $sloc))
}

let law_heading :=
| title = LAW_HEADING ; {
  let (title, id, is_archive, precedence) = title in {
    law_heading_name = (title, Pos.from_lpos $sloc);
    law_heading_id = id;
    law_heading_is_archive = is_archive;
    law_heading_precedence = precedence;
  }
}

let law_text :=
| lines = nonempty_list(LAW_TEXT) ; { String.trim (String.concat "" lines) }

let source_file_item :=
| text = law_text ; { LawText text }
| BEGIN_CODE ;
  ~ = code ;
  text = END_CODE ; {
  CodeBlock (code, (text, Pos.from_lpos $sloc), false)
}
| heading = law_heading ; {
  LawHeading (heading, [])
}
| code = metadata_block ; {
  let (code, source_repr) = code in
  CodeBlock (code, source_repr, true)
}
| BEGIN_DIRECTIVE ; LAW_INCLUDE ; COLON ;
  args = nonempty_list(DIRECTIVE_ARG) ;
  page = option(AT_PAGE) ;
  END_DIRECTIVE ; {
  let filename = String.trim (String.concat "" args) in
  let pos = Pos.from_lpos $sloc in
  let jorftext = Re.Pcre.regexp "(JORFARTI\\d{12}|LEGIARTI\\d{12}|CETATEXT\\d{12})" in
  if Re.Pcre.pmatch ~rex:jorftext filename && page = None then
    LawInclude (Ast.LegislativeText (filename, pos))
  else if Filename.extension filename = ".pdf" || page <> None then
    LawInclude (Ast.PdfFile ((filename, pos), page))
  else
    LawInclude (Ast.CatalaFile (filename, pos))
}

let source_file :=
| hd = source_file_item ; tl = source_file ; { hd::tl }
| EOF ; { [] }
