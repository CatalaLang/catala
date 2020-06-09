(*
  This file is part of the Catala compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2020 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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
  open Ast
%}

%token EOF
%token<string * string option * string option> LAW_ARTICLE
%token<string * int> LAW_HEADING
%token<Ast.law_include> LAW_INCLUDE
%token<string> LAW_TEXT
%token<string> CONSTRUCTOR IDENT
%token<string> END_CODE
%token<int> INT_LITERAL
%token TRUE FALSE
%token<int * int> DECIMAL_LITERAL
%token<int * int> MONEY_AMOUNT
%token BEGIN_CODE TEXT MASTER_FILE
%token COLON ALT DATA VERTICAL
%token OF INTEGER COLLECTION
%token RULE CONDITION DEFINED_AS
%token EXISTS IN SUCH THAT NOW LESSER GREATER
%token DOT AND OR LPAREN RPAREN OPTIONAL EQUAL
%token CARDINAL LESSER_EQUAL GREATER_EQUAL
%token ASSERTION FIXED BY YEAR
%token PLUS MINUS MULT DIV MATCH WITH VARIES WITH_V
%token FOR ALL WE_HAVE INCREASING DECREASING
%token NOT BOOLEAN PERCENT ARROW
%token SCOPE FILLED NOT_EQUAL DEFINITION
%token STRUCT CONTENT IF THEN DEPENDS DECLARATION
%token CONTEXT ENUM ELSE DATE SUM
%token BEGIN_METADATA END_METADATA MONEY DECIMAL
%token UNDER_CONDITION CONSEQUENCE

%type <Ast.source_file_or_master> source_file_or_master

%start source_file_or_master

%%

typ_base:
| INTEGER { (Integer, $sloc) }
| BOOLEAN { (Boolean, $sloc) }
| MONEY { (Money, $sloc) }
| TEXT { (Text, $sloc) }
| DECIMAL { (Decimal, $sloc) }
| DATE { (Date, $sloc) }
| c = constructor {
  let (s, _) = c in
  (Named s, $sloc)
}

collection_marked:
| COLLECTION { $sloc }

optional_marked:
| OPTIONAL { $sloc }

typ:
| t = typ_base {
  let t, loc = t in
  (Primitive t, loc)
}
| collection_marked t = typ {
  (Optional t, $sloc)
}
| optional_marked t = typ {
  (Collection t, $sloc)
}

qident_prefix:
| c = constructor DOT { c }

qident:
| p = option(qident_prefix) b = separated_nonempty_list(DOT, ident) {
  ({
    qident_prefix = p;
    qident_path = b;
    } ,$sloc)
}

atomic_expression:
| q = qident { let (q, q_pos) = q in (Qident q, q_pos) }
| l = literal { let (l, l_pos) = l in (Literal l, l_pos) }
| LPAREN e = expression RPAREN { e }

small_expression:
| e = atomic_expression { e }
| e = small_expression ARROW c = constructor {
  (Project (e, c), $sloc)
}

constructor_payload:
| CONTENT e = small_expression { e }

primitive_expression:
| e = small_expression { e }
| NOW { (Builtin Now, $sloc) }
| CARDINAL {
   (Builtin Cardinal, $sloc)
}
| c = constructor p = option(constructor_payload) {
  (Inject (c, p), $sloc)
}

num_literal:
| d = INT_LITERAL { (Int d, $sloc) }
| d = DECIMAL_LITERAL {
  let (d1, d2) = d in
  (Dec (d1, d2), $sloc)
 }

unit_literal:
| PERCENT { (Percent, $sloc) }
| YEAR { (Year, $sloc)}

date_int:
| d = INT_LITERAL { (d, $sloc) }

literal:
| l = num_literal u = option(unit_literal) {
   (Number (l, u), $sloc)
}
| money = MONEY_AMOUNT {
  let (units, cents) = money in
  (MoneyAmount {
    money_amount_units = units;
    money_amount_cents = cents;
  }, $sloc)
}
| VERTICAL d = date_int DIV m = date_int DIV y = date_int VERTICAL {
  (Date {
    literal_date_day = d;
    literal_date_month = m;
    literal_date_year = y;
  }, $sloc) 
}
| TRUE { (Bool true, $sloc) }
| FALSE { (Bool false, $sloc) }

compare_op:
| LESSER { (Lt, $sloc) }
| LESSER_EQUAL { (Lte, $sloc) }
| GREATER { (Gt, $sloc) }
| GREATER_EQUAL { (Gte, $sloc) }
| EQUAL { (Eq, $sloc) }
| NOT_EQUAL { (Neq, $sloc) }

aggregate_func:
| SUM { (Aggregate AggregateSum, $sloc) }
| CARDINAL { (Aggregate AggregateCount, $sloc) }

aggregate:
| func = aggregate_func FOR i = ident IN e1 = primitive_expression
  OF e2 = base_expression {
  (CollectionOp (func, i, e1, e2), $sloc)
}

base_expression:
| e = primitive_expression { e }
| ag = aggregate { ag }
| e1 = primitive_expression OF e2 = base_expression {
  (FunCall (e1, e2), $sloc)
}
| e = primitive_expression WITH c= constructor {
  (TestMatchCase (e, c), $sloc)
}
| e1 = primitive_expression IN e2 = base_expression {
   (MemCollection (e1, e2), $sloc)
}

mult_op:
| MULT { (Mult, $sloc) }
| DIV { (Div, $sloc) }

mult_expression:
| e =  base_expression { e }
| e1 = base_expression binop = mult_op e2  = mult_expression {
  (Binop (binop, e1, e2), $sloc)
}

sum_op:
| PLUS { (Add, $sloc) }
| MINUS { (Sub, $sloc) }

sum_unop:
| MINUS { (Minus, $sloc) }

sum_expression:
| e = mult_expression { e }
| e1 = mult_expression binop = sum_op e2 = sum_expression {
  (Binop (binop, e1, e2), $sloc)
}
| unop = sum_unop e = sum_expression { (Unop (unop, e), $sloc) }

logical_op:
| AND { (And, $sloc) }
| OR { (Or, $sloc) }

logical_unop:
| NOT { (Not, $sloc) }

compare_expression:
| e = sum_expression { e }
| e1 = sum_expression binop = compare_op e2 = compare_expression {
  (Binop (binop, e1, e2), $sloc)
 }

logical_expression:
| e = compare_expression { e }
| unop = logical_unop e = compare_expression { (Unop (unop, e), $sloc) }
| e1 = compare_expression binop = logical_op e2 = logical_expression {
   (Binop (binop, e1, e2), $sloc)
 }

optional_binding:
| { ([], None)}
| OF i = ident {([], Some i)}
| OF c = constructor cs_and_i = constructor_binding {
  let (cs, i) = cs_and_i in
  (c::cs, i)
}

constructor_binding:
| c = constructor cs_and_i = optional_binding {
  let (cs, i) = cs_and_i in
  (c::cs, i)
 }

match_arm:
| pat = constructor_binding COLON e = logical_expression {
  ({
    (* DM 14/04/2020 : I can't have the $sloc in constructor_binding... *)
    match_case_pattern = (pat, $sloc);
    match_case_expr = e;
    }, $sloc)
}

match_arms:
| ALT a = match_arm arms = match_arms {
  let (arms, _) = arms in
   (a::arms, $sloc)
}
| { ([], $sloc)}

for_all_marked:
| FOR ALL { $sloc }

exists_marked:
| EXISTS { $sloc }

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
  (CollectionOp ((Exists, pos), i, e1, e2), $sloc)
}
| i_in_e1 = forall_prefix e2 = expression {
  let (pos, i,e1) = i_in_e1 in
  (CollectionOp ((Forall, pos), i, e1, e2), $sloc)
}
| MATCH e = primitive_expression WITH arms = match_arms {
  (MatchWith (e, arms), $sloc)
}
| IF e1 = expression THEN e2 = expression ELSE e3 = base_expression {
  (IfThenElse (e1, e2, e3), $sloc)
}
| e = logical_expression { e }

condition:
| UNDER_CONDITION e = expression { e }

condition_consequence:
| cond = condition CONSEQUENCE { cond }

rule_expr:
| i = qident p = option(definition_parameters) { (i, p) }

rule:
| name_and_param = rule_expr cond = option(condition_consequence)
   consequence = option(NOT) FILLED {
    let (name, param_applied) = name_and_param in
    ({
      rule_parameter = param_applied;
      rule_condition = cond;
      rule_name = name;
      rule_consequence = match consequence with Some _ -> false | None -> true
      }, $sloc)
  }

definition_parameters:
| OF i = ident { i }

definition:
| name = qident param = option(definition_parameters)
  cond = option(condition_consequence) DEFINED_AS e = expression {
    ({
      definition_name = name;
      definition_parameter = param;
      definition_condition = cond;
      definition_expr = e;
      }, $sloc)
  }

variation_type:
| INCREASING { (Increasing, $sloc) }
| DECREASING { (Decreasing, $sloc) }

assertion_base:
| e = expression { let (e, _) = e in (e, $sloc) }

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
| RULE r = rule {
   let (r, _) = r in (Rule r, $sloc)
}
| DEFINITION d = definition {
  let (d, _) = d in (Definition d, $sloc)
 }
| ASSERTION contents = assertion {
  (contents, $sloc)
}

ident:
| i = IDENT { (i, $sloc) }

condition_pos:
| CONDITION { $sloc }

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
    | Some (return_typ, return_pos) -> (Func  {
      arg_typ = (typ, typ_pos);
      return_typ = (Data return_typ, return_pos);
    }, $sloc) ;
  }, $sloc)
}

scope_decl_context_condition:
| UNDER_CONDITION e = expression { e }

scope_decl_item:
| CONTEXT i = ident CONTENT t = typ func_typ = option(struct_scope_func) { (ContextData ({
  scope_decl_context_item_name = i;
  scope_decl_context_item_typ =
    let (typ, typ_pos) = t in
    match func_typ with
    | None -> (Base (Data typ), typ_pos)
    | Some (return_typ, return_pos) -> (Func  {
      arg_typ = (Data typ, typ_pos);
      return_typ = (Data return_typ, return_pos);
    }, $sloc);
  }), $sloc) }
| CONTEXT i = ident SCOPE c = constructor e = option(scope_decl_context_condition) {
  (ContextScope({
    scope_decl_context_scope_name = i;
    scope_decl_context_scope_sub_scope = c;
    scope_decl_context_scope_condition = e;
  }), $sloc)
}
| CONTEXT i = ident _condition = CONDITION func_typ = option(struct_scope_func) { (ContextData ({
  scope_decl_context_item_name = i;
  scope_decl_context_item_typ =
    match func_typ with
    | None -> (Base (Condition), $loc(_condition))
    | Some (return_typ, return_pos) -> (Func  {
      arg_typ = (Condition, $loc(_condition));
      return_typ = (Data return_typ, return_pos);
    }, $sloc);
  }), $sloc) }

enum_decl_line_payload:
| CONTENT t = typ { let (t, t_pos) = t in (Base (Data t), t_pos) }

enum_decl_line:
| ALT c = constructor t = option(enum_decl_line_payload) { ({
    enum_decl_case_name = c;
    enum_decl_case_typ = t;
  }, $sloc) }

constructor:
| c = CONSTRUCTOR { (c, $sloc) }

scope_use_condition:
| UNDER_CONDITION e = expression { e }

code_item:
| SCOPE c = constructor e = option(scope_use_condition) COLON items = nonempty_list(scope_item) {
  (ScopeUse {
    scope_use_name = c;
    scope_use_condition = e;
    scope_use_items = items;
  }, $sloc)
}
| DECLARATION STRUCT c = constructor COLON scopes = list(struct_scope) {
  (StructDecl {
    struct_decl_name = c;
    struct_decl_fields = scopes;
  }, $sloc)
}
| DECLARATION SCOPE c = constructor COLON context = nonempty_list(scope_decl_item) {
  (ScopeDecl {
      scope_decl_name = c;
      scope_decl_context = context;
  }, $sloc)
}
| DECLARATION ENUM c = constructor COLON cases = nonempty_list(enum_decl_line) {
  (EnumDecl {
    enum_decl_name = c;
    enum_decl_cases = cases;
  }, $sloc)
}

code:
| code = list(code_item) { (code, $sloc) }

metadata_block:
| BEGIN_CODE code_and_pos = code text = END_CODE END_METADATA {
  let (code, pos) = code_and_pos in
  (code, (text, pos))
}

source_file_item:
| title = LAW_ARTICLE {
  let (title, id, exp_date) = title in LawArticle {
    law_article_name = (title, $sloc);
    law_article_id = id;
    law_article_expiration_date = exp_date;
  }
}
| heading = LAW_HEADING { let (title, precedence) = heading in LawHeading (title, precedence) }
| text = LAW_TEXT { LawText text }
| BEGIN_METADATA code = metadata_block {
  let (code, source_repr) = code in
  MetadataBlock (code, source_repr)
}
| BEGIN_CODE code_and_pos = code text = END_CODE {
  let (code, pos) = code_and_pos in
  CodeBlock (code, (text, pos))
}
| includ = LAW_INCLUDE {
  LawInclude includ
}

source_file:
| i = source_file_item f = source_file { i::f }
| EOF { [] }

master_file_include:
| includ = LAW_INCLUDE {
  match includ with
  | CatalaFile (file, _) -> (file, $sloc)
  | _ -> Errors.parser_error $sloc "inclusion" (Printf.sprintf "Include in master file must be .catala file!" )
}

master_file_includes:
| i = master_file_include is = master_file_includes { i::is }
| EOF { [] }

source_file_or_master:
| MASTER_FILE is = master_file_includes { MasterFile is }
| f = source_file { SourceFile f }
