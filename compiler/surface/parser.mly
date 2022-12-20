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

%type <Ast.source_file> source_file

%start source_file

(* The token is returned for every line of law text, make them right-associative
   so that we concat them efficiently as much as possible. *)
%right LAW_TEXT

(* Precedence of expression constructions *)
%right top_expr
%right ALT
%right let_expr IS
%right logical_expr AND
%right logical_or_expr OR XOR
%nonassoc compare_expr GREATER GREATER_EQUAL LESSER LESSER_EQUAL EQUAL NOT_EQUAL
%left sum_expr PLUS MINUS PLUSPLUS
%left mult_expr MULT DIV
%right apply OF CONTAINS FOR SUCH WITH
%right unop_expr
%right CONTENT
%left DOT
%%

let pos(x) ==
| ~=x ; { Pos.from_lpos $loc }

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
| c = UIDENT ; <Named>

let typ :=
| t = typ_base ; <Primitive>
| COLLECTION ; t = addpos(typ) ; <Collection>

let qident ==
| b = separated_nonempty_list(DOT, ident) ; <>

(* let path :=
 * | { [] } %prec qpath
 * | ~=constructor ; DOT ; ~=path ; <List.cons> %prec qpath *)
(* Not yet supported, at the moment it's just an option: *)
let path ==
| { None }
| ~=constructor ; DOT ; <Some>

let expression ==
| e = addpos(naked_expression) ; { (e: expression) }

let naked_expression :=
| q = LIDENT ; {
    (match Localisation.lex_builtin q with
     | Some b -> Builtin b
     | None -> Ident q)
}
| l = literal ; {
  Literal l
}
| LPAREN ; e = expression ; RPAREN ; <Marked.unmark>
| e = expression ;
  DOT ; c = path ;
  i = ident ; {
  Dotted (e, c, i)
}
| CARDINAL ; {
  Builtin Cardinal
}
| DECIMAL ; {
  Builtin ToDecimal
}
| MONEY ; {
  Builtin ToMoney
}
| LBRACKET ; l = separated_list(SEMICOLON, expression) ; RBRACKET ; {
  ArrayLit l
}
| e = struct_or_enum_inject ; <>
| e1 = expression ;
  OF ;
  e2 = expression ; {
  FunCall (e1, e2)
} %prec apply
| OUTPUT ; OF ;
  c = constructor ;
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
  FOR ; i = ident ;
  AMONG ; coll = expression ; {
  CollectionOp (Map {f = i, f}, coll)
} %prec apply
| max = minmax ;
  OF ; coll = expression ;
  OR ; IF ; COLLECTION ; EMPTY ; THEN ;
  default = expression ; {
  CollectionOp (AggregateExtremum { max; default }, coll)
} %prec apply
| op = unop ; e = expression ; {
  Unop (op, e)
} %prec unop_expr
| e1 = expression ;
  binop = mult_op ;
  e2 = expression ; {
  Binop (binop, e1, e2)
} %prec mult_expr
| e1 = expression ;
  binop = sum_op ;
  e2 = expression ; {
  Binop (binop, e1, e2)
} %prec sum_expr
| e1 = expression ;
  binop = compare_op ;
  e2 = expression ; {
  Binop (binop, e1, e2)
} %prec compare_expr
| e1 = expression ;
  binop = logical_or_op ;
  e2 = expression ; {
  Binop (binop, e1, e2)
} %prec logical_or_expr
| e1 = expression ;
  binop = logical_and_op ;
  e2 = expression ; {
  Binop (binop, e1, e2)
} %prec logical_expr
| EXISTS ; i = ident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; predicate = expression ; {
  CollectionOp (Exists {predicate = i, predicate}, coll)
} %prec let_expr
| FOR ; ALL ; i = ident ;
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
| LET ; id = ident ;
  DEFINED_AS ; e1 = expression ;
  IN ; e2 = expression ; {
  LetIn (id, e1, e2)
} %prec let_expr
| i = ident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; f = expression ; {
  CollectionOp (Filter {f = i, f}, coll)
} %prec top_expr
| fmap = expression ;
  FOR ; i = ident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; ffilt = expression ; {
  CollectionOp (Map {f = i, fmap}, (CollectionOp (Filter {f = i, ffilt}, coll), Pos.from_lpos $loc))
} %prec top_expr
| i = ident ;
  AMONG ; coll = expression ;
  SUCH ; THAT ; f = expression ;
  IS ; max = minmax ;
  OR ; IF ; COLLECTION ; EMPTY ; THEN ; default = expression ; {
  CollectionOp (AggregateArgExtremum { max; default; f = i, f }, coll)
} %prec top_expr


let struct_content_field :=
| field = ident ; COLON ; e = expression ; <>

let enum_content_opt :=
| {None} %prec CONTENT
| CONTENT ; ~ = expression ; <Some> %prec CONTENT

let struct_or_enum_inject ==
| ~ = path ;
  ~ = constructor ;
  data = enum_content_opt ; {
  EnumInject(path, constructor, data)
}
| _ = path ;
  c = constructor ;
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

let compare_op :=
| LESSER ; { (Lt KPoly, Pos.from_lpos $sloc) }
| LESSER_EQUAL ; { (Lte KPoly, Pos.from_lpos $sloc) }
| GREATER ; { (Gt KPoly, Pos.from_lpos $sloc) }
| GREATER_EQUAL ; { (Gte KPoly, Pos.from_lpos $sloc) }
| EQUAL ; { (Eq, Pos.from_lpos $sloc) }
| NOT_EQUAL ; { (Neq, Pos.from_lpos $sloc) }

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
| NOT ; { (Not, Pos.from_lpos $sloc) }
| k = MINUS ; { (Minus k, Pos.from_lpos $sloc) }

let mult_op ==
| k = MULT ; { (Mult k, Pos.from_lpos $sloc) }
| k = DIV ; { (Div k, Pos.from_lpos $sloc) }

let sum_op ==
| k = PLUS ; { (Add k, Pos.from_lpos $sloc) }
| k = MINUS ; { (Sub k, Pos.from_lpos $sloc) }
| PLUSPLUS ; { (Concat, Pos.from_lpos $sloc) }

let logical_and_op ==
| AND ; { (And, Pos.from_lpos $sloc) }

let logical_or_op ==
| OR ; { (Or, Pos.from_lpos $sloc) }
| XOR ; { (Xor, Pos.from_lpos $sloc) }

let constructor_binding :=
| ~ = path; ~ = constructor ; OF ; ~ = ident ; {
  ([path, constructor], Some ident)
}
| ~ = path; ~ = constructor ; {
  ([path, constructor], None)
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

let condition ==
| UNDER_CONDITION ; e = expression ; <>

let condition_consequence :=
| cond = condition ; CONSEQUENCE ; { cond }

let rule_expr :=
| i = addpos(qident) ; p = option(definition_parameters) ; <>

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
  }, $sloc
}

let definition_parameters :=
| OF ; i = ident ; { i }

let label :=
| LABEL ; i = ident ; { i }

let state :=
| STATE ; s = ident ; { s }

let exception_to :=
| EXCEPTION ; i = option(ident) ; {
  match i with
  | None -> UnlabeledException
  | Some x -> ExceptionToLabel x
}

let definition :=
| label = option(label);
  except = option(exception_to) ;
  DEFINITION ;
  name = addpos(qident) ;
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
  }, $sloc
}

let variation_type :=
| INCREASING ; { (Increasing, Pos.from_lpos $sloc) }
| DECREASING ; { (Decreasing, Pos.from_lpos $sloc) }

let assertion_base :=
| e = expression ; { let (e, _) = e in (e, Pos.from_lpos $sloc) }

let assertion :=
| cond = option(condition_consequence) ;
  base = assertion_base ; {
  (Assertion {
    assertion_condition = cond;
    assertion_content = base;
  })
}
| FIXED ; q = addpos(qident) ; BY ; i = ident ; {
  MetaAssertion (FixedBy (q, i))
}
| VARIES ; q = addpos(qident) ;
  WITH_V ; e = expression ;
  t = option(variation_type) ; {
  MetaAssertion (VariesWith (q, e, t))
}

let scope_item :=
| r = rule ; {
 let (r, _) = r in (Rule r, Pos.from_lpos $sloc)
}
| d = definition ; {
 let (d, _) = d in (Definition d, Pos.from_lpos $sloc)
}
| ASSERTION ; contents = assertion ; {
  (contents, Pos.from_lpos $sloc)
}

let ident :=
| i = LIDENT ; {
 match Localisation.lex_builtin i with
 | Some _ ->
     Errors.raise_spanned_error
       (Pos.from_lpos $sloc)
       "Reserved builtin name"
 | None ->
     (i, Pos.from_lpos $sloc)
}

let condition_pos :=
| CONDITION ; { Pos.from_lpos $sloc }

let struct_scope_base :=
| DATA ; i = ident ;
  CONTENT ; t = addpos(typ) ; {
  let t, pos = t in
  (i, (Data t, pos))
}
| pos = condition_pos ; i = ident ; {
  (i, (Condition, pos))
}

let struct_scope_func ==
| DEPENDS ; t = addpos(typ) ; { t }

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
 }, Pos.from_lpos $sloc
}

let scope_decl_item_attribute_input :=
| CONTEXT ; { Context, Pos.from_lpos $sloc }
| INPUT ; { Input, Pos.from_lpos $sloc }

let scope_decl_item_attribute_output :=
| OUTPUT ; { true, Pos.from_lpos $sloc }
| { false, Pos.from_lpos $sloc }

let scope_decl_item_attribute :=
| input = scope_decl_item_attribute_input ;
  output = scope_decl_item_attribute_output ; {
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
  i = ident ;
  CONTENT ; t = addpos(typ) ;
  func_typ = option(struct_scope_func) ;
  states = list(state) ; {
  (ContextData {
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
  }, Pos.from_lpos $sloc)
}
| i = ident ; SCOPE ; c = constructor ; {
  (ContextScope{
    scope_decl_context_scope_name = i;
    scope_decl_context_scope_sub_scope = c;
    scope_decl_context_scope_attribute = {
      scope_decl_context_io_input = (Internal, Pos.from_lpos $sloc);
      scope_decl_context_io_output = (false, Pos.from_lpos $sloc);
    };
  }, Pos.from_lpos $sloc)
}
| attr = scope_decl_item_attribute ;
  i = ident ;
  _condition = CONDITION ;
  func_typ = option(struct_scope_func) ;
  states = list(state) ; {
  ContextData {
    scope_decl_context_item_name = i;
    scope_decl_context_item_attribute = attr;
    scope_decl_context_item_typ =
      (match func_typ with
      | None -> (Base (Condition), Pos.from_lpos $loc(_condition))
      | Some (arg_typ, arg_pos) ->
        Func {
          arg_typ = (Data arg_typ, arg_pos);
          return_typ = (Condition, Pos.from_lpos $loc(_condition));
        }, Pos.from_lpos $sloc);
    scope_decl_context_item_states = states;
  }, Pos.from_lpos $sloc
}

let enum_decl_line :=
| ALT ; c = constructor ;
  t = option(preceded(CONTENT,addpos(typ))) ; {
  {
    enum_decl_case_name = c;
    enum_decl_case_typ =
      Option.map (fun (t, t_pos) ->  Base (Data t), t_pos) t;
  }, Pos.from_lpos $sloc
}

let constructor :=
| ~ = addpos(UIDENT) ; <>

let scope_use_condition :=
| UNDER_CONDITION ; e = expression ; <>

let code_item :=
| SCOPE ; c = constructor ;
  e = option(scope_use_condition) ;
  COLON ; items = nonempty_list(scope_item) ; {
  (ScopeUse {
    scope_use_name = c;
    scope_use_condition = e;
    scope_use_items = items;
  }, Pos.from_lpos $sloc)
}
| DECLARATION ; STRUCT ; c = constructor ;
  COLON ; scopes = list(struct_scope) ; {
  (StructDecl {
    struct_decl_name = c;
    struct_decl_fields = scopes;
  }, Pos.from_lpos $sloc)
}
| DECLARATION ; SCOPE ; c = constructor ;
  COLON ; context = nonempty_list(scope_decl_item) ; {
  (ScopeDecl {
    scope_decl_name = c;
    scope_decl_context = context;
  }, Pos.from_lpos $sloc)
}
| DECLARATION ; ENUM ; c = constructor ;
  COLON ; cases = list(enum_decl_line) ; {
  (EnumDecl {
    enum_decl_name = c;
    enum_decl_cases = cases;
  }, Pos.from_lpos $sloc)
}

let code :=
| code = list(code_item) ; { (code, Pos.from_lpos $sloc) }

let metadata_block :=
| BEGIN_METADATA ; option(law_text) ;
  code_and_pos = code ;
  text = END_CODE ; {
  let (code, _) = code_and_pos in
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
  code_and_pos = code ;
  text = END_CODE ; {
  let (code, _) = code_and_pos in
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
  let jorftext = Re.Pcre.regexp "JORFTEXT\\d{12}" in
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
