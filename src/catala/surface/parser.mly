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
  open Utils
  %}

%token EOF
%token<string * string option * string option * int> LAW_ARTICLE
%token<string * int> LAW_HEADING
%token<Ast.law_include> LAW_INCLUDE
%token<string> LAW_TEXT
%token<string> CONSTRUCTOR IDENT
%token<string> END_CODE
%token<Runtime.integer> INT_LITERAL
%token TRUE FALSE
%token<Runtime.integer * Runtime.integer> DECIMAL_LITERAL
%token<Runtime.integer * Runtime.integer> MONEY_AMOUNT
%token BEGIN_CODE TEXT MASTER_FILE
%token COLON ALT DATA VERTICAL
%token OF INTEGER COLLECTION
%token RULE CONDITION DEFINED_AS
%token LESSER GREATER LESSER_EQUAL GREATER_EQUAL
%token LESSER_DEC GREATER_DEC LESSER_EQUAL_DEC GREATER_EQUAL_DEC
%token LESSER_MONEY GREATER_MONEY LESSER_EQUAL_MONEY GREATER_EQUAL_MONEY
%token LESSER_DATE GREATER_DATE LESSER_EQUAL_DATE GREATER_EQUAL_DATE
%token LESSER_DURATION GREATER_DURATION LESSER_EQUAL_DURATION GREATER_EQUAL_DURATION
%token EXISTS IN SUCH THAT
%token DOT AND OR XOR LPAREN RPAREN EQUAL
%token CARDINAL ASSERTION FIXED BY YEAR MONTH DAY
%token PLUS MINUS MULT DIV
%token PLUSDEC MINUSDEC MULTDEC DIVDEC
%token PLUSMONEY MINUSMONEY MULTMONEY DIVMONEY
%token MINUSDATE PLUSDATE PLUSDURATION MINUSDURATION
%token MATCH WITH VARIES WITH_V
%token FOR ALL WE_HAVE INCREASING DECREASING
%token NOT BOOLEAN PERCENT DURATION
%token SCOPE FILLED NOT_EQUAL DEFINITION
%token STRUCT CONTENT IF THEN DEPENDS DECLARATION
%token CONTEXT ENUM ELSE DATE SUM
%token BEGIN_METADATA END_METADATA MONEY DECIMAL
%token UNDER_CONDITION CONSEQUENCE LBRACKET RBRACKET
%token LABEL EXCEPTION LSQUARE RSQUARE SEMICOLON
%token INT_TO_DEC MAXIMUM MINIMUM INIT
%token GET_DAY GET_MONTH GET_YEAR
%token FILTER MAP

%type <Ast.source_file_or_master> source_file_or_master

%start source_file_or_master

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
| q = ident { let (q, q_pos) = q in (Ident q, q_pos) }
| l = literal { let (l, l_pos) = l in (Literal l, l_pos) }
| LPAREN e = expression RPAREN { e }

small_expression:
| e = atomic_expression { e }
| e = small_expression DOT c = option(terminated(constructor,DOT)) i = ident {
  (Dotted (e, c, i), Pos.from_lpos $sloc)
}

struct_content_field:
| field = ident COLON e = logical_expression {
  (field, e)
}

enum_inject_content:
| CONTENT e = small_expression { e }

struct_inject_content:
| LBRACKET ALT fields = separated_nonempty_list(ALT, struct_content_field) RBRACKET { fields }

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
  | CARDINAL {
    (Builtin Cardinal, Pos.from_lpos $sloc)
  }

  | INT_TO_DEC {
    (Builtin IntToDec, Pos.from_lpos $sloc)
  }
  | GET_DAY {
    (Builtin GetDay, Pos.from_lpos $sloc)
  }
  | GET_MONTH {
    (Builtin GetMonth, Pos.from_lpos $sloc)
  }
  | GET_YEAR {
    (Builtin GetYear, Pos.from_lpos $sloc)
  }
  | e = struct_or_enum_inject {
    e
  }
  | LSQUARE l = separated_list(SEMICOLON, expression) RSQUARE {
    (ArrayLit l, Pos.from_lpos $sloc)
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

  date_int:
  | d = INT_LITERAL { (Runtime.integer_to_int d, Pos.from_lpos $sloc) }

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
  | VERTICAL d = date_int DIV m = date_int DIV y = date_int VERTICAL {
    (LDate {
      literal_date_day = (match !Utils.Cli.locale_lang with `En -> m | `Fr -> d);
    literal_date_month = (match !Utils.Cli.locale_lang with `En -> d | `Fr -> m);
    literal_date_year = y;
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
    (Aggregate (AggregateArgExtremum (true, Pos.unmark t, init)), Pos.from_lpos $sloc)
  }
  | CONTENT MINIMUM t = typ_base INIT init = primitive_expression {
    (Aggregate (AggregateArgExtremum (false, Pos.unmark t, init)), Pos.from_lpos $sloc)
  }
  | MAXIMUM t = typ_base INIT init = primitive_expression {
    (Aggregate (AggregateExtremum (true, Pos.unmark t, init)), Pos.from_lpos $sloc)
  }
  | MINIMUM t = typ_base INIT init = primitive_expression {
    (Aggregate (AggregateExtremum (false, Pos.unmark t, init)), Pos.from_lpos $sloc)
  }
  | SUM t = typ_base { (Aggregate (AggregateSum (Pos.unmark t)), Pos.from_lpos $sloc) }
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
  | e1 = primitive_expression OF e2 = base_expression {
    (FunCall (e1, e2), Pos.from_lpos $sloc)
  }
  | e = primitive_expression WITH c = constructor_binding {
    (TestMatchCase (e, (c, Pos.from_lpos $sloc)), Pos.from_lpos $sloc)
  }
  | e1 = primitive_expression IN e2 = base_expression {
    (MemCollection (e1, e2), Pos.from_lpos $sloc)
  }

  mult_op:
  | MULT { (Mult KInt, Pos.from_lpos $sloc) }
  | DIV { (Div KInt, Pos.from_lpos $sloc) }
  | MULTDEC { (Mult KDec, Pos.from_lpos $sloc) }
  | DIVDEC { (Div KDec, Pos.from_lpos $sloc) }
  | MULTMONEY { (Mult KMoney, Pos.from_lpos $sloc) }
  | DIVMONEY { (Div KMoney, Pos.from_lpos $sloc) }

  mult_expression:
  | e =  base_expression { e }
  | e1 = base_expression binop = mult_op e2  = mult_expression {
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

  sum_unop:
  | MINUS { (Minus KInt, Pos.from_lpos $sloc) }
  | MINUSDEC { (Minus KDec, Pos.from_lpos $sloc) }
  | MINUSMONEY { (Minus KMoney, Pos.from_lpos $sloc) }
  | MINUSDURATION { (Minus KDuration, Pos.from_lpos $sloc) }

  sum_expression:
  | e = mult_expression { e }
  | e1 = mult_expression binop = sum_op e2 = sum_expression {
    (Binop (binop, e1, e2), Pos.from_lpos $sloc)
  }
  | unop = sum_unop e = sum_expression { (Unop (unop, e), Pos.from_lpos $sloc) }

  logical_op:
  | AND { (And, Pos.from_lpos $sloc) }
  | OR { (Or, Pos.from_lpos $sloc) }
  | XOR { (Xor, Pos.from_lpos $sloc) }

  logical_unop:
  | NOT { (Not, Pos.from_lpos $sloc) }

  compare_expression:
  | e = sum_expression { e }
  | e1 = sum_expression binop = compare_op e2 = compare_expression {
    (Binop (binop, e1, e2), Pos.from_lpos $sloc)
  }

  logical_expression:
  | e = compare_expression { e }
  | unop = logical_unop e = compare_expression { (Unop (unop, e), Pos.from_lpos $sloc) }
  | e1 = compare_expression binop = logical_op e2 = logical_expression {
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
  | pat = constructor_binding COLON e = logical_expression {
    ({
      (* DM 14/04/2020 : I can't have the $sloc in constructor_binding... *)
      match_case_pattern = (pat, Pos.from_lpos $sloc);
    match_case_expr = e;
  }, Pos.from_lpos $sloc)
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
  | IF e1 = expression THEN e2 = expression ELSE e3 = base_expression {
    (IfThenElse (e1, e2, e3), Pos.from_lpos $sloc)
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
  consequence = rule_consequence {
    let (name, param_applied) = name_and_param in
    let cons : bool Pos.marked = consequence in
    let rule_exception = match except with | None -> NotAnException | Some x -> x in
    ({
      rule_label = label;
      rule_exception_to = rule_exception;
      rule_parameter = param_applied;
      rule_condition = cond;
      rule_name = name;
      rule_consequence = cons;
  }, $sloc)
    }

  definition_parameters:
  | OF i = ident { i }

  label:
  | LABEL i = ident { i }

  exception_to:
  | EXCEPTION i = option(ident) {
    match i with | None -> UnlabeledException | Some x -> ExceptionToLabel x  }

  definition:
  | label = option(label)
  except = option(exception_to)
  DEFINITION
  name = qident param = option(definition_parameters)
  cond = option(condition_consequence) DEFINED_AS e = expression {
    let def_exception = match except with | None -> NotAnException | Some x -> x in
    ({
      definition_label = label;
      definition_exception_to = def_exception;
      definition_name = name;
      definition_parameter = param;
      definition_condition = cond;
      definition_expr = e;
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
  | i = IDENT { (i, Pos.from_lpos $sloc) }

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

  scope_decl_item:
    | CONTEXT i = ident CONTENT t = typ func_typ = option(struct_scope_func) { (ContextData ({
      scope_decl_context_item_name = i;
  scope_decl_context_item_typ =
    let (typ, typ_pos) = t in
    match func_typ with
    | None -> (Base (Data typ), typ_pos)
    | Some (arg_typ, arg_pos) -> (Func  {
      arg_typ = (Data arg_typ, arg_pos);
      return_typ = (Data typ, typ_pos);
  }, Pos.from_lpos $sloc);
    }), Pos.from_lpos $sloc) }
    | CONTEXT i = ident SCOPE c = constructor {
      (ContextScope({
        scope_decl_context_scope_name = i;
    scope_decl_context_scope_sub_scope = c;
    }), Pos.from_lpos $sloc)
      }
    | CONTEXT i = ident _condition = CONDITION func_typ = option(struct_scope_func) { (ContextData ({
      scope_decl_context_item_name = i;
  scope_decl_context_item_typ =
    match func_typ with
    | None -> (Base (Condition), Pos.from_lpos $loc(_condition))
    | Some (arg_typ, arg_pos) -> (Func  {
      arg_typ = (Data arg_typ, arg_pos);
      return_typ = (Condition, Pos.from_lpos $loc(_condition));
    }, Pos.from_lpos $sloc);
    }), Pos.from_lpos $sloc) }

    enum_decl_line_payload:
    | CONTENT t = typ { let (t, t_pos) = t in (Base (Data t), t_pos) }

    enum_decl_line:
    | ALT c = constructor t = option(enum_decl_line_payload) { ({
      enum_decl_case_name = c;
    enum_decl_case_typ = t;
    }, Pos.from_lpos $sloc) }

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
    | DECLARATION ENUM c = constructor COLON cases = nonempty_list(enum_decl_line) {
      (EnumDecl {
        enum_decl_name = c;
    enum_decl_cases = cases;
    }, Pos.from_lpos $sloc)
      }

    code:
    | code = list(code_item) { (code, Pos.from_lpos $sloc) }

    metadata_block:
    | BEGIN_CODE option(law_text) code_and_pos = code text = END_CODE option(law_text) END_METADATA {
   let (code, pos) = code_and_pos in
   (code, (text, pos))
    }

    law_article_item:
    | text = law_text { LawText text }
    | BEGIN_CODE code_and_pos = code text = END_CODE  {
   let (code, pos) = code_and_pos in
   CodeBlock (code, (text, pos))
    }

    law_article:
    | title = LAW_ARTICLE {
   let (title, id, exp_date, precedence) = title in {
     law_article_name = (title, Pos.from_lpos $sloc);
    law_article_id = id;
    law_article_expiration_date = exp_date;
    law_article_precedence = precedence;
    }
   }

    law_heading:
    | heading = LAW_HEADING { let (title, precedence) = heading in  {
      law_heading_name = title;
    law_heading_precedence = precedence;
    }
    }

    law_articles_items:
    | hd = law_article_item tl = law_articles_items{ hd::tl }
    | { [] }

    law_text:
    | text = LAW_TEXT { String.trim text }

    law_intermediate_text:
    | text = law_text { LawStructure (IntermediateText text) }


    source_file_article:
    | article = law_article items = law_articles_items  {
      LawStructure (LawArticle (article, items))
    }

    source_file_item:
    | heading = law_heading {
      LawStructure (LawHeading (heading, []))
    }
    | BEGIN_METADATA option(law_text) code = metadata_block {
   let (code, source_repr) = code in
   LawStructure (MetadataBlock (code, source_repr))
    }
    | includ = LAW_INCLUDE {
      LawStructure (LawInclude includ)
    }

    source_file_after_text:
    | i = source_file_article f = source_file_after_text {
      i::f
    }
    | i = source_file_item l = list(law_intermediate_text) f = source_file_after_text {
      i::l@f
    }
    | EOF { [] }

    source_file:
    | l = list(law_intermediate_text) f = source_file_after_text { l@f }

    master_file_include:
    | includ = LAW_INCLUDE {
      match includ with
  | CatalaFile (file, _) -> (file, Pos.from_lpos $sloc)
  | _ -> Errors.raise_spanned_error
    (Printf.sprintf "Include in master file must be .catala file!")
    (Pos.from_lpos $sloc)
    }

    master_file_includes:
  | i = master_file_include option(law_text) is = master_file_includes { i::is }
  | EOF { [] }

  source_file_or_master:
  | MASTER_FILE option(law_text) is = master_file_includes { MasterFile is }
  | f = source_file { SourceFile (f) }
