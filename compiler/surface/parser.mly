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
  open Utils
%}

%parameter<Localisation: sig
  val lex_builtin: string -> Ast.builtin_expression option
end>

%type <Ast.source_file> source_file

%start source_file

(* The token is returned for every line of law text, make them right-associative
   so that we concat them efficiently as much as possible. *)
%right LAW_TEXT

%%

typ_base:
| INTEGER { (Integer, Pos.from_lpos $sloc) }
| BOOLEAN { (Boolean, Pos.from_lpos $sloc) }
| MONEY { (Money, Pos.from_lpos $sloc) }
| DURATION { (Duration, Pos.from_lpos $sloc) }
| TEXT { (Text, Pos.from_lpos $sloc) }
| DECIMAL { (Decimal, Pos.from_lpos $sloc) }
| DATE { (Date, Pos.from_lpos $sloc) }
| c = constructor {
  let (s, _) = c in
  (Named s, Pos.from_lpos $sloc)
}

collection_marked:
| COLLECTION { Pos.from_lpos $sloc }

typ:
| t = typ_base {
  let t, loc = t in
  (Primitive t, loc)
}
| collection_marked t = typ {
  (Collection t, Pos.from_lpos $sloc)
}

qident:
| b = separated_nonempty_list(DOT, ident) {
  ( b, Pos.from_lpos $sloc)
}

atomic_expression:
| q = IDENT {
    (match Localisation.lex_builtin q with
     | Some b -> Builtin b
     | None -> Ident q),
    Pos.from_lpos $sloc }
| l = literal { let (l, l_pos) = l in (Literal l, l_pos) }
| LPAREN e = expression RPAREN { e }

small_expression:
| e = atomic_expression { e }
| e = small_expression DOT c = option(terminated(constructor,DOT)) i = ident {
  (Dotted (e, c, i), Pos.from_lpos $sloc)
}
| CARDINAL {
  (Builtin Cardinal, Pos.from_lpos $sloc)
}
| LSQUARE l = separated_list(SEMICOLON, expression) RSQUARE {
  (ArrayLit l, Pos.from_lpos $sloc)
}

struct_content_field:
| field = ident COLON e = logical_expression {
  (field, e)
}

enum_inject_content:
| CONTENT e = small_expression { e }

struct_inject_content:
| LBRACKET fields = nonempty_list(preceded(ALT, struct_content_field)) RBRACKET { fields }

struct_or_enum_inject:
| enum = constructor c = option(preceded(DOT, constructor)) data = option(enum_inject_content) {
  (* The fully qualified enum is actually the optional part, but it leads to shift/reduce conflicts.
     We flip it here *)
  match c with
  | None -> (EnumInject(None, enum, data), Pos.from_lpos $sloc)
  | Some c -> (EnumInject(Some enum, c, data), Pos.from_lpos $sloc)
}
| c = constructor fields = struct_inject_content { (StructLit(c, fields), Pos.from_lpos $sloc) }

primitive_expression:
| e = small_expression { e }
| e = struct_or_enum_inject {
  e
}

num_literal:
| d = INT_LITERAL { (Int d, Pos.from_lpos $sloc) }
| d = DECIMAL_LITERAL {
let (d1, d2) = d in
(Dec (d1, d2), Pos.from_lpos $sloc)
}

unit_literal:
| PERCENT { (Percent, Pos.from_lpos $sloc) }
| YEAR { (Year, Pos.from_lpos $sloc)}
| MONTH { (Month, Pos.from_lpos $sloc) }
| DAY { (Day, Pos.from_lpos $sloc) }

literal:
| l = num_literal u = option(unit_literal) {
  (LNumber (l, u), Pos.from_lpos $sloc)
}
| money = MONEY_AMOUNT {
  let (units, cents) = money in
  (LMoneyAmount {
    money_amount_units = units;
    money_amount_cents = cents;
  }, Pos.from_lpos $sloc)
}
| VERTICAL d = DATE_LITERAL VERTICAL {
  let (y,m,d) = d in
  (LDate {
    literal_date_year = y;
    literal_date_month = m;
    literal_date_day = d;
  }, Pos.from_lpos $sloc)
}
| TRUE { (LBool true, Pos.from_lpos $sloc) }
| FALSE { (LBool false, Pos.from_lpos $sloc) }

compare_op:
| LESSER { (Lt KInt, Pos.from_lpos $sloc) }
| LESSER_EQUAL { (Lte KInt, Pos.from_lpos $sloc) }
| GREATER { (Gt KInt, Pos.from_lpos $sloc) }
| GREATER_EQUAL { (Gte KInt, Pos.from_lpos $sloc) }
| LESSER_DEC { (Lt KDec, Pos.from_lpos $sloc) }
| LESSER_EQUAL_DEC { (Lte KDec, Pos.from_lpos $sloc) }
| GREATER_DEC { (Gt KDec, Pos.from_lpos $sloc) }
| GREATER_EQUAL_DEC { (Gte KDec, Pos.from_lpos $sloc) }
| LESSER_MONEY { (Lt KMoney, Pos.from_lpos $sloc) }
| LESSER_EQUAL_MONEY { (Lte KMoney, Pos.from_lpos $sloc) }
| GREATER_MONEY { (Gt KMoney, Pos.from_lpos $sloc) }
| GREATER_EQUAL_MONEY { (Gte KMoney, Pos.from_lpos $sloc) }
| LESSER_DATE { (Lt KDate, Pos.from_lpos $sloc) }
| LESSER_EQUAL_DATE { (Lte KDate, Pos.from_lpos $sloc) }
| GREATER_DATE { (Gt KDate, Pos.from_lpos $sloc) }
| GREATER_EQUAL_DATE { (Gte KDate, Pos.from_lpos $sloc) }
| LESSER_DURATION { (Lt KDuration, Pos.from_lpos $sloc) }
| LESSER_EQUAL_DURATION { (Lte KDuration, Pos.from_lpos $sloc) }
| GREATER_DURATION { (Gt KDuration, Pos.from_lpos $sloc) }
| GREATER_EQUAL_DURATION { (Gte KDuration, Pos.from_lpos $sloc) }
| EQUAL { (Eq, Pos.from_lpos $sloc) }
| NOT_EQUAL { (Neq, Pos.from_lpos $sloc) }

aggregate_func:
| CONTENT MAXIMUM t = typ_base INIT init = primitive_expression {
  (Aggregate (AggregateArgExtremum (true, Marked.unmark t, init)), Pos.from_lpos $sloc)
}
| CONTENT MINIMUM t = typ_base INIT init = primitive_expression {
  (Aggregate (AggregateArgExtremum (false, Marked.unmark t, init)), Pos.from_lpos $sloc)
}
| MAXIMUM t = typ_base INIT init = primitive_expression {
  (Aggregate (AggregateExtremum (true, Marked.unmark t, init)), Pos.from_lpos $sloc)
}
| MINIMUM t = typ_base INIT init = primitive_expression {
  (Aggregate (AggregateExtremum (false, Marked.unmark t, init)), Pos.from_lpos $sloc)
}
| SUM t = typ_base { (Aggregate (AggregateSum (Marked.unmark t)), Pos.from_lpos $sloc) }
| CARDINAL { (Aggregate AggregateCount, Pos.from_lpos $sloc) }
| FILTER { (Filter, Pos.from_lpos $sloc ) }
| MAP { (Map, Pos.from_lpos $sloc) }

aggregate:
| func = aggregate_func FOR i = ident IN e1 = primitive_expression
  OF e2 = base_expression {
  (CollectionOp (func, i, e1, e2), Pos.from_lpos $sloc)
}

base_expression:
| e = primitive_expression { e }
| ag = aggregate { ag }
| e1 = small_expression OF e2 = base_expression {
  (FunCall (e1, e2), Pos.from_lpos $sloc)
}
| c = constructor OF
  LBRACKET fields = list(preceded (ALT, struct_content_field)) RBRACKET {
  (* empty list is allowed *)
  (ScopeCall (c, fields), Pos.from_lpos $sloc)
}
| e = primitive_expression WITH c = constructor_binding {
  (TestMatchCase (e, (c, Pos.from_lpos $sloc)), Pos.from_lpos $sloc)
}
| e1 = primitive_expression CONTAINS e2 = base_expression {
  (MemCollection (e2, e1), Pos.from_lpos $sloc)
}

unop:
| NOT { (Not, Pos.from_lpos $sloc) }
| MINUS { (Minus KInt, Pos.from_lpos $sloc) }
| MINUSDEC { (Minus KDec, Pos.from_lpos $sloc) }
| MINUSMONEY { (Minus KMoney, Pos.from_lpos $sloc) }
| MINUSDURATION { (Minus KDuration, Pos.from_lpos $sloc) }

unop_expression:
| e = base_expression { e }
| op = unop e = unop_expression { (Unop (op, e), Pos.from_lpos $sloc) }

mult_op:
| MULT { (Mult KInt, Pos.from_lpos $sloc) }
| DIV { (Div KInt, Pos.from_lpos $sloc) }
| MULTDEC { (Mult KDec, Pos.from_lpos $sloc) }
| DIVDEC { (Div KDec, Pos.from_lpos $sloc) }
| MULTMONEY { (Mult KMoney, Pos.from_lpos $sloc) }
| DIVMONEY { (Div KMoney, Pos.from_lpos $sloc) }
| MULDURATION { (Mult KDuration, Pos.from_lpos $sloc) }

mult_expression:
| e =  unop_expression { e }
| e1 = mult_expression binop = mult_op e2  = unop_expression {
  (Binop (binop, e1, e2), Pos.from_lpos $sloc)
}

sum_op:
| PLUSDURATION { (Add KDuration, Pos.from_lpos $sloc) }
| MINUSDURATION { (Sub KDuration, Pos.from_lpos $sloc) }
| PLUSDATE { (Add KDate, Pos.from_lpos $sloc) }
| MINUSDATE { (Sub KDate, Pos.from_lpos $sloc) }
| PLUSMONEY { (Add KMoney, Pos.from_lpos $sloc) }
| MINUSMONEY { (Sub KMoney, Pos.from_lpos $sloc) }
| PLUSDEC { (Add KDec, Pos.from_lpos $sloc) }
| MINUSDEC { (Sub KDec, Pos.from_lpos $sloc) }
| PLUS { (Add KInt, Pos.from_lpos $sloc) }
| MINUS { (Sub KInt, Pos.from_lpos $sloc) }
| PLUSPLUS { (Concat, Pos.from_lpos $sloc) }

sum_expression:
| e = mult_expression { e }
| e1 = sum_expression binop = sum_op e2 = mult_expression {
  (Binop (binop, e1, e2), Pos.from_lpos $sloc)
}

logical_and_op:
| AND { (And, Pos.from_lpos $sloc) }

logical_or_op:
| OR { (Or, Pos.from_lpos $sloc) }
| XOR { (Xor, Pos.from_lpos $sloc) }

compare_expression:
| e = sum_expression { e }
| e1 = sum_expression binop = compare_op e2 = compare_expression {
  (Binop (binop, e1, e2), Pos.from_lpos $sloc)
}

logical_atom:
| e = compare_expression { e }

logical_or_expression:
| e = logical_atom { e }
| e1 = logical_atom binop = logical_or_op e2 = logical_or_expression {
  (Binop (binop, e1, e2), Pos.from_lpos $sloc)
}

logical_expression:
| e = logical_or_expression { e }
| e1 = logical_or_expression binop = logical_and_op e2 = logical_expression {
  (Binop (binop, e1, e2), Pos.from_lpos $sloc)
}

maybe_qualified_constructor:
| c_or_path = constructor c = option(preceded(DOT, constructor)) {
  match c with
  | None -> (None, c_or_path)
  | Some c -> (Some c_or_path, c)
}

optional_binding:
| { ([], None)}
| OF i = ident {([], Some i)}
| OF c = maybe_qualified_constructor cs_and_i = constructor_binding {
  let (cs, i) = cs_and_i in
  (c::cs, i)
}

constructor_binding:
| c = maybe_qualified_constructor cs_and_i = optional_binding {
let (cs, i) = cs_and_i in
(c::cs, i)
}

match_arm:
| WILDCARD COLON e = logical_expression { (WildCard (e), Pos.from_lpos $sloc) }
| pat = constructor_binding COLON e = logical_expression {
  (MatchCase ({
    (* DM 14/04/2020 : I can't have the $sloc in constructor_binding... *)
    match_case_pattern = (pat, Pos.from_lpos $sloc);
    match_case_expr = e;
  }), Pos.from_lpos $sloc)
}

match_arms:
| ALT a = match_arm arms = match_arms {
let (arms, _) = arms in
(a::arms, Pos.from_lpos $sloc)
}
| { ([], Pos.from_lpos $sloc)}

for_all_marked:
| FOR ALL { Pos.from_lpos $sloc }

exists_marked:
| EXISTS { Pos.from_lpos $sloc }

forall_prefix:
| pos = for_all_marked i = ident IN e = primitive_expression WE_HAVE {
  (pos, i, e)
}

exists_prefix:
| pos = exists_marked i = ident IN e = primitive_expression SUCH THAT {
  (pos, i, e)
}

expression:
| i_in_e1 = exists_prefix e2 = expression {
let (pos, i,e1) = i_in_e1 in
(CollectionOp ((Exists, pos), i, e1, e2), Pos.from_lpos $sloc)
}
| i_in_e1 = forall_prefix e2 = expression {
let (pos, i,e1) = i_in_e1 in
(CollectionOp ((Forall, pos), i, e1, e2), Pos.from_lpos $sloc)
}
| MATCH e = primitive_expression WITH arms = match_arms {
  (MatchWith (e, arms), Pos.from_lpos $sloc)
}
| IF e1 = expression THEN e2 = expression ELSE e3 = expression {
  (IfThenElse (e1, e2, e3), Pos.from_lpos $sloc)
}
| LET id = ident DEFINED_AS e1 = expression IN e2 = expression {
  (LetIn (id, e1, e2), Pos.from_lpos $sloc)
}
| e = logical_expression { e }

condition:
| UNDER_CONDITION e = expression { e }

condition_consequence:
| cond = condition CONSEQUENCE { cond }

rule_expr:
| i = qident p = option(definition_parameters) { (i, p) }

rule_consequence:
| flag = option(NOT) FILLED {
  let b = match flag with Some _ -> false | None -> true in
  (b, Pos.from_lpos $sloc)
}

rule:
| label = option(label)
  except = option(exception_to)
  RULE
  name_and_param = rule_expr cond = option(condition_consequence)
  state = option(state)
  consequence = rule_consequence {
    let (name, param_applied) = name_and_param in
    let cons : bool Marked.pos = consequence in
    let rule_exception = match except with | None -> NotAnException | Some x -> x in
    ({
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
  }, $sloc)
}

definition_parameters:
| OF i = ident { i }

label:
| LABEL i = ident { i }

state:
| STATE s = ident { s }

exception_to:
| EXCEPTION i = option(ident) {
  match i with | None -> UnlabeledException | Some x -> ExceptionToLabel x
}

definition:
| label = option(label)
  except = option(exception_to)
  DEFINITION
  name = qident param = option(definition_parameters)
  state = option(state)
  cond = option(condition_consequence) DEFINED_AS e = expression {
    let def_exception = match except with | None -> NotAnException | Some x -> x in
    ({
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
    }, $sloc)
}

variation_type:
| INCREASING { (Increasing, Pos.from_lpos $sloc) }
| DECREASING { (Decreasing, Pos.from_lpos $sloc) }

assertion_base:
| e = expression { let (e, _) = e in (e, Pos.from_lpos $sloc) }

assertion:
| cond = option(condition_consequence) base = assertion_base {
  (Assertion {
    assertion_condition = cond;
    assertion_content = base;
  })
}
| FIXED q = qident BY i = ident { MetaAssertion (FixedBy (q, i)) }
| VARIES q = qident WITH_V e = base_expression t = option(variation_type) {
  MetaAssertion (VariesWith (q, e, t))
}

scope_item:
| r = rule {
 let (r, _) = r in (Rule r, Pos.from_lpos $sloc)
}
| d = definition {
 let (d, _) = d in (Definition d, Pos.from_lpos $sloc)
}
| ASSERTION contents = assertion {
  (contents, Pos.from_lpos $sloc)
}

ident:
| i = IDENT {
 match Localisation.lex_builtin i with
 | Some _ ->
    Errors.raise_spanned_error
      (Pos.from_lpos $sloc)
      "Reserved builtin name"
 | None ->
    (i, Pos.from_lpos $sloc)
}

condition_pos:
| CONDITION { Pos.from_lpos $sloc }

struct_scope_base:
| DATA i= ident CONTENT t = typ {
  let t, pos = t in
  (i, (Data t, pos))
}
| pos = condition_pos i = ident {
  (i, (Condition, pos))
}

struct_scope_func:
| DEPENDS t = typ { t }

struct_scope:
| name_and_typ = struct_scope_base func_typ = option(struct_scope_func) {
  let (name, typ) = name_and_typ in
  let (typ, typ_pos) = typ in
  ({
    struct_decl_field_name = name;
    struct_decl_field_typ = match func_typ with
    | None -> (Base typ, typ_pos)
    | Some (arg_typ, arg_pos) -> (Func  {
      arg_typ = (Data arg_typ, arg_pos);
      return_typ = (typ, typ_pos);
    }, Pos.from_lpos $sloc) ;
  }, Pos.from_lpos $sloc)
}

scope_decl_item_attribute_input:
| CONTEXT { Context, Pos.from_lpos $sloc }
| INPUT { Input, Pos.from_lpos $sloc }

scope_decl_item_attribute_output:
| OUTPUT { true, Pos.from_lpos $sloc }
| { false, Pos.from_lpos $sloc }

scope_decl_item_attribute:
| input = scope_decl_item_attribute_input
  output = scope_decl_item_attribute_output {
    {
      scope_decl_context_io_input = input;
      scope_decl_context_io_output = output
    }
  }
| INTERNAL {
    {
      scope_decl_context_io_input = (Internal, Pos.from_lpos $sloc);
      scope_decl_context_io_output = (false, Pos.from_lpos $sloc)
    }
  }
| OUTPUT {
    {
      scope_decl_context_io_input = (Internal, Pos.from_lpos $sloc);
      scope_decl_context_io_output = (true, Pos.from_lpos $sloc)
    }
  }


scope_decl_item:
| attr = scope_decl_item_attribute
  i = ident
  CONTENT t = typ func_typ = option(struct_scope_func)
  states = list(state)
{ (ContextData ({
  scope_decl_context_item_name = i;
  scope_decl_context_item_attribute = attr;
  scope_decl_context_item_typ =
    (let (typ, typ_pos) = t in
    match func_typ with
    | None -> (Base (Data typ), typ_pos)
    | Some (arg_typ, arg_pos) -> (Func  {
      arg_typ = (Data arg_typ, arg_pos);
      return_typ = (Data typ, typ_pos);
    }, Pos.from_lpos $sloc));
  scope_decl_context_item_states = states;
  }), Pos.from_lpos $sloc)
}
| i = ident SCOPE c = constructor {
  (ContextScope({
    scope_decl_context_scope_name = i;
    scope_decl_context_scope_sub_scope = c;
    scope_decl_context_scope_attribute = {
      scope_decl_context_io_input = (Internal, Pos.from_lpos $sloc);
      scope_decl_context_io_output = (false, Pos.from_lpos $sloc);
    };
  }), Pos.from_lpos $sloc)
}
| attr = scope_decl_item_attribute
  i = ident _condition = CONDITION func_typ = option(struct_scope_func)
  states = list(state)
{
  (ContextData ({
    scope_decl_context_item_name = i;
    scope_decl_context_item_attribute = attr;
    scope_decl_context_item_typ =
      (match func_typ with
      | None -> (Base (Condition), Pos.from_lpos $loc(_condition))
      | Some (arg_typ, arg_pos) -> (Func  {
        arg_typ = (Data arg_typ, arg_pos);
        return_typ = (Condition, Pos.from_lpos $loc(_condition));
      }, Pos.from_lpos $sloc));
    scope_decl_context_item_states = states;
    }), Pos.from_lpos $sloc
  )
}

enum_decl_line_payload:
| CONTENT t = typ { let (t, t_pos) = t in (Base (Data t), t_pos) }

enum_decl_line:
| ALT c = constructor t = option(enum_decl_line_payload) {
  ({
    enum_decl_case_name = c;
    enum_decl_case_typ = t;
  }, Pos.from_lpos $sloc)
}

constructor:
| c = CONSTRUCTOR { (c, Pos.from_lpos $sloc) }

scope_use_condition:
| UNDER_CONDITION e = expression { e }

code_item:
| SCOPE c = constructor e = option(scope_use_condition) COLON items = nonempty_list(scope_item) {
  (ScopeUse {
    scope_use_name = c;
    scope_use_condition = e;
    scope_use_items = items;
  }, Pos.from_lpos $sloc)
}
| DECLARATION STRUCT c = constructor COLON scopes = list(struct_scope) {
  (StructDecl {
    struct_decl_name = c;
    struct_decl_fields = scopes;
  }, Pos.from_lpos $sloc)
}
| DECLARATION SCOPE c = constructor COLON context = nonempty_list(scope_decl_item) {
  (ScopeDecl {
    scope_decl_name = c;
    scope_decl_context = context;
  }, Pos.from_lpos $sloc)
}
| DECLARATION ENUM c = constructor COLON cases = list(enum_decl_line) {
  (EnumDecl {
    enum_decl_name = c;
    enum_decl_cases = cases;
  }, Pos.from_lpos $sloc)
}

code:
| code = list(code_item) { (code, Pos.from_lpos $sloc) }

metadata_block:
|  BEGIN_METADATA option(law_text) code_and_pos = code text = END_CODE {
  let (code, _) = code_and_pos in
   (code, (text, Pos.from_lpos $sloc))
}


law_heading:
| title = LAW_HEADING {
  let (title, id, is_archive, precedence) = title in {
    law_heading_name = (title, Pos.from_lpos $sloc);
    law_heading_id = id;
    law_heading_is_archive = is_archive;
    law_heading_precedence = precedence;
  }
}

law_text:
| lines = nonempty_list(LAW_TEXT) { String.trim (String.concat "" lines) }

source_file_item:
| text = law_text { LawText text }
| BEGIN_CODE code_and_pos = code text = END_CODE  {
  let (code, _) = code_and_pos in
  CodeBlock (code, (text, Pos.from_lpos $sloc), false)
}
| heading = law_heading {
  LawHeading (heading, [])
}
| code = metadata_block {
  let (code, source_repr) = code in
  CodeBlock (code, source_repr, true)
}
| BEGIN_DIRECTIVE LAW_INCLUDE COLON args = nonempty_list(DIRECTIVE_ARG) page = option(AT_PAGE) END_DIRECTIVE {
  let filename = String.trim (String.concat "" args) in
  let pos = Pos.from_lpos $sloc in
  let jorftext = Re.Pcre.regexp "JORFTEXT\\d{12}" in
  if Re.Pcre.pmatch ~rex:jorftext filename && page = None then
    LawInclude (Ast.LegislativeText (filename, pos))
  else if Filename.extension filename = ".pdf" || page <> None then
    LawInclude (Ast.PdfFile ((filename, pos), page))
  else
    LawInclude (Ast.CatalaFile (filename, pos))
}


source_file:
| hd = source_file_item tl = source_file { hd:: tl }
| EOF { [] }
