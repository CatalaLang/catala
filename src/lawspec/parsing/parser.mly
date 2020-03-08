(*
  This file is part of the Lawspec compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

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
%token<string> LAW_ARTICLE
%token<string> LAW_CODE
%token<string> LAW_TEXT
%token<string> CONSTRUCTOR IDENT
%token<string> END_CODE
%token BEGIN_CODE CHOICE
%token COLON ALT POINT SITUATION SOURCE DATA
%token OF SEMICOLON

%type <Ast.source_file> source_file

%start source_file

%%

choice:
| CONSTRUCTOR {}

choices:
| ALT choice choices {}
| {}

situation_type:
| CHOICE IDENT {}

situation:
| DATA IDENT OF situation_type {}

code_item:
| CHOICE IDENT COLON choices POINT { }
| SITUATION CONSTRUCTOR SOURCE IDENT COLON situation POINT { }

code:
| code_item code {}
| {}

source_file_item:
| title = LAW_ARTICLE { LawArticle title }
| code = LAW_CODE { LawCode code }
| text = LAW_TEXT { LawText text }
| BEGIN_CODE code text = END_CODE { CodeBlock text }

source_file:
| i = source_file_item f = source_file { i::f }
| EOF { [] }
