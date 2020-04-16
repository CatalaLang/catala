(*
  This file is part of the Lawspec compiler, a specification language for tax and social benefits
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
  open Parse_utils
%}

%token EOF
%token<string> LAW_ARTICLE
%token<string * int> LAW_HEADING
%token<string> LAW_TEXT
%token<string> CONSTRUCTOR IDENT
%token<string> END_CODE
%token<int> INT_LITERAL
%token<int * int> DECIMAL_LITERAL
%token BEGIN_CODE
%token COLON ALT DATA VERTICAL
%token OF INTEGER COLLECTION
%token RULE CONDITION DEFINED_AS
%token EXISTS IN SUCH THAT NOW LESSER GREATER
%token DOT AND OR LPAREN RPAREN OPTIONAL EQUAL
%token CARDINAL LESSER_EQUAL GREATER_EQUAL
%token ASSERTION FIXED BY YEAR
%token PLUS MINUS MULT DIV MATCH WITH VARIES_WITH
%token FOR ALL WE_HAVE INCREASING DECREASING
%token NOT BOOLEAN PERCENT ARROW
%token FIELD FILLED EURO NOT_EQUAL DEFINITION
%token STRUCT CONTENT IF THEN DEPENDS DECLARATION
%token CONTEXT INCLUDES ENUM ELSE DATE SUM
%token BEGIN_METADATA END_METADATA MONEY DECIMAL
%token UNDER_CONDITION CONSEQUENCE

%type <Ast.source_file> source_file

%start source_file

%%

typ_base:
| INTEGER { (Integer, mk_position $sloc) }
| BOOLEAN { (Boolean, mk_position $sloc) }
| MONEY { (Money, mk_position $sloc) }
| DECIMAL { (Decimal, mk_position $sloc) }
| DATE { (Date, mk_position $sloc) }
| c = constructor {
  let (s, _) = c in
  (Named s, mk_position $sloc)
}

collection_marked:
| COLLECTION { mk_position $sloc }

optional_marked:
| OPTIONAL { mk_position $sloc }

typ:
| collection = option(collection_marked) t = typ_base optional = option(optional_marked) {
  (Data {
    typ_data_collection = collection;
    typ_data_optional = optional;
    typ_data_base = t;
  }, mk_position $sloc)
}

qident:
| i = ident { let (i, i_pos) = i in ([Ident i, i_pos], mk_position $sloc) }
| i = ident DOT q = qident {
  let (i, i_pos) = i in
  let (q, _) = q in
  ((Ident i, i_pos)::q, mk_position $sloc)
}
| c = constructor DOT q = qident {
  let (c, c_pos) = c in
  let (q, _) = q in
  ((Constructor c, c_pos)::q, mk_position $sloc)
}

atomic_expression:
| q = qident { let (q, q_pos) = q in (Qident q, q_pos) }
| l = literal { let (l, l_pos) = l in (Literal l, l_pos) }
| LPAREN e = expression RPAREN { e }

small_expression:
| e = atomic_expression { e }
| e = small_expression ARROW c = constructor {
  (Project (e, c), mk_position $sloc)
}

constructor_payload:
| CONTENT e = small_expression { e }

primitive_expression:
| e = small_expression { e }
| NOW { (Builtin Now, mk_position $sloc) }
| CARDINAL {
   (Builtin Cardinal, mk_position $sloc)
}
| c = constructor p = option(constructor_payload) {
  (Inject (c, p), mk_position $sloc)
}

num_literal:
| d = INT_LITERAL { (Int d, mk_position $sloc) }
| d = DECIMAL_LITERAL {
  let (d1, d2) = d in
  (Dec (d1, d2), mk_position $sloc)
 }

unit_literal:
| PERCENT { (Percent, mk_position $sloc) }
| EURO { (Euro, mk_position $sloc) }
| YEAR { (Year, mk_position $sloc)}

date_int:
| d = INT_LITERAL { (d, mk_position $sloc) }

literal:
| l = num_literal u = option(unit_literal) {
   (Number (l, u), mk_position $sloc)
}
| VERTICAL d = date_int DIV m = date_int DIV y = date_int VERTICAL {
  (Date {
    literal_date_day = d;
    literal_date_month = m;
    literal_date_year = y;
    }, mk_position $sloc)
}


compare_op:
| LESSER { (Lt, mk_position $sloc) }
| LESSER_EQUAL { (Lte, mk_position $sloc) }
| GREATER { (Gt, mk_position $sloc) }
| GREATER_EQUAL { (Gte, mk_position $sloc) }
| EQUAL { (Eq, mk_position $sloc) }
| NOT_EQUAL { (Neq, mk_position $sloc) }

aggregate_func:
| SUM { (Aggregate AggregateSum, mk_position $sloc) }
| CARDINAL { (Aggregate AggregateCount, mk_position $sloc) }

aggregate:
| func = aggregate_func FOR i = ident IN e1 = primitive_expression
  OF e2 = base_expression {
  (CollectionOp (func, i, e1, e2), mk_position $sloc)
}

base_expression:
| e = primitive_expression { e }
| ag = aggregate { ag }
| e1 = primitive_expression OF e2 = base_expression {
  (FunCall (e1, e2), mk_position $sloc)
}
| e = primitive_expression WITH c= constructor {
  (TestMatchCase (e, c), mk_position $sloc)
}
| e1 = primitive_expression IN e2 = base_expression {
   (MemCollection (e1, e2), mk_position $sloc)
}

mult_op:
| MULT { (Mult, mk_position $sloc) }
| DIV { (Div, mk_position $sloc) }

mult_expression:
| e =  base_expression { e }
| e1 = base_expression binop = mult_op e2  = mult_expression {
  (Binop (binop, e1, e2), mk_position $sloc)
}

sum_op:
| PLUS { (Add, mk_position $sloc) }
| MINUS { (Sub, mk_position $sloc) }

sum_unop:
| MINUS { (Minus, mk_position $sloc) }

sum_expression:
| e = mult_expression { e }
| e1 = mult_expression binop = sum_op e2 = sum_expression {
  (Binop (binop, e1, e2), mk_position $sloc)
}
| unop = sum_unop e = sum_expression { (Unop (unop, e), mk_position $sloc) }

logical_op:
| AND { (And, mk_position $sloc) }
| OR { (Or, mk_position $sloc) }

logical_unop:
| NOT { (Not, mk_position $sloc) }

compare_expression:
| e = sum_expression { e }
| e1 = sum_expression binop = compare_op e2 = compare_expression {
  (Binop (binop, e1, e2), mk_position $sloc)
 }

logical_expression:
| e = compare_expression { e }
| unop = logical_unop e = compare_expression { (Unop (unop, e), mk_position $sloc) }
| e1 = compare_expression binop = logical_op e2 = logical_expression {
   (Binop (binop, e1, e2), mk_position $sloc)
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
    match_case_pattern = (pat, mk_position $sloc);
    match_case_expr = e;
    }, mk_position $sloc)
}

match_arms:
| ALT a = match_arm arms = match_arms {
  let (arms, _) = arms in
   (a::arms, mk_position $sloc)
}
| { ([], mk_position $sloc)}

for_all_marked:
| FOR ALL { mk_position $sloc }

exists_marked:
| EXISTS { mk_position $sloc }

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
  (CollectionOp ((Exists, pos), i, e1, e2), mk_position $sloc)
}
| i_in_e1 = forall_prefix e2 = expression {
  let (pos, i,e1) = i_in_e1 in
  (CollectionOp ((Forall, pos), i, e1, e2), mk_position $sloc)
}
| MATCH e = primitive_expression WITH arms = match_arms {
  (MatchWith (e, arms), mk_position $sloc)
}
| IF e1 = expression THEN e2 = expression ELSE e3 = base_expression {
  (IfThenElse (e1, e2, e3), mk_position $sloc)
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
   FILLED {
    let (name, param_applied) = name_and_param in
    ({
      rule_parameter = param_applied;
      rule_condition = cond;
      rule_name = name;
      }, mk_position $sloc)
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
      }, mk_position $sloc)
  }

variation_type:
| INCREASING { (Increasing, mk_position $sloc) }
| DECREASING { (Decreasing, mk_position $sloc) }

assertion_base:
| e = expression { let (e, _) = e in (Assert e, mk_position $sloc) }
| q = qident FIXED BY i = ident { (FixedBy (q, i), mk_position $sloc) }
| q = qident VARIES_WITH e = base_expression t = option(variation_type) {
  (VariesWith (q, e, t), mk_position $sloc)
}

assertion:
| cond = option(condition_consequence) base = assertion_base {
  (cond, base)
}

application_field_item:
| RULE r = rule {
   let (r, _) = r in (Rule r, mk_position $sloc)
}
| DEFINITION d = definition {
  let (d, _) = d in (Definition d, mk_position $sloc)
 }
| ASSERTION contents = assertion {
  (let (cond, cont) = contents in Assertion {
    assertion_condition = cond;
    assertion_content = cont;
  }, mk_position $sloc)
}

ident:
| i = IDENT { (i, mk_position $sloc) }

condition_pos:
| CONDITION { mk_position $sloc }

struct_field_base:
| DATA i= ident CONTENT t = typ {
  (i, t)
}
| pos = condition_pos i = ident {
  (i, (Condition, pos))
}

struct_field_func:
| DEPENDS OF t = typ { t }

struct_field:
| name_and_typ = struct_field_base func_typ = option(struct_field_func) {
  let (name, typ) = name_and_typ in
  let (typ, typ_pos) = typ in
  ({
    struct_decl_field_name = name;
    struct_decl_field_typ = match func_typ with
    | None -> (Base typ, typ_pos)
    | Some (return_typ, return_pos) -> (Func  {
      arg_typ = (typ, typ_pos);
      return_typ = (return_typ, return_pos);
    }, mk_position $sloc) ;
  }, mk_position $sloc)
}

field_decl_item:
| CONTEXT i = ident CONTENT t = typ func_typ = option(struct_field_func) { ({
  field_decl_context_item_name = i;
  field_decl_context_item_typ =
    let (typ, typ_pos) = t in
    match func_typ with
    | None -> (Base typ, typ_pos)
    | Some (return_typ, return_pos) -> (Func  {
      arg_typ = (typ, typ_pos);
      return_typ = (return_typ, return_pos);
    }, mk_position $sloc);
  }, mk_position $sloc) }

field_decl_include:
| c1 = constructor DOT i1 = ident EQUAL c2 = constructor DOT i2 = ident {
  ({
    parent_field_name = c1;
    parent_field_context_item = i1 ;
    sub_field_name = c2;
    sub_field_context_item = i2;
  }, mk_position $sloc)
}

field_decl_includes_context:
| CONTEXT join = nonempty_list(field_decl_include) { join }

field_decl_includes:
| INCLUDES FIELD c = constructor context = option(field_decl_includes_context) {
 ({
   field_decl_include_sub_field = c;
   field_decl_include_joins = match context with
   | None -> []
   | Some context -> context
  }, mk_position $sloc)
}

enum_decl_line_payload:
| CONTENT t = typ { let (t, t_pos) = t in (Base t, t_pos) }

enum_decl_line:
| ALT c = constructor t = option(enum_decl_line_payload) { ({
    enum_decl_case_name = c;
    enum_decl_case_typ = t;
  }, mk_position $sloc) }

constructor:
| c = CONSTRUCTOR { (c, mk_position $sloc) }

code_item:
| FIELD c = constructor COLON items = nonempty_list(application_field_item) {
  (FieldUse {
    field_use_name = c;
    field_use_items = items;
  }, mk_position $sloc)
}
| DECLARATION STRUCT c = constructor COLON fields = list(struct_field) {
  (StructDecl {
    struct_decl_name = c;
    struct_decl_fields = fields;
  }, mk_position $sloc)
}
| DECLARATION FIELD c = constructor COLON context = nonempty_list(field_decl_item)
  includes = list(field_decl_includes) {
  (FieldDecl {
      field_decl_name = c;
      field_decl_context = context;
      field_decl_includes = includes;
  }, mk_position $sloc)
}
| DECLARATION ENUM c = constructor COLON cases = nonempty_list(enum_decl_line) {
  (EnumDecl {
    enum_decl_name = c;
    enum_decl_cases = cases;
  }, mk_position $sloc)
}

code:
| code = list(code_item) { (code, mk_position $sloc) }

metadata_block:
| BEGIN_CODE code_and_pos = code text = END_CODE END_METADATA {
  let (code, pos) = code_and_pos in
  (code, (text, pos))
}

source_file_item:
| title = LAW_ARTICLE { LawArticle title }
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

source_file:
| i = source_file_item f = source_file { i::f }
| EOF { [] }
