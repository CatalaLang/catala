/* This file has been generated by the Catala compiler, do not edit! */

#include <stdio.h>
#include <stdlib.h>
#include <runtime.c>

enum option_2_enum_code {
  option_2_enum_none_2_cons,
  option_2_enum_some_2_cons
} option_2_enum_code;

typedef struct option_2_enum {
  enum option_2_enum_code code;
  union {
    CATALA_UNIT none_2_cons;
    CATALA_DEC some_2_cons;
  } payload;
} option_2_enum;

typedef struct foo_struct {
  CATALA_BOOL x_field;
  CATALA_DEC y_field;
} foo_struct;

typedef struct array_3_struct {
  CATALA_DEC * content_field;
  CATALA_INT length_field;
} array_3_struct;

typedef struct array_2_struct {
  option_2_enum * content_field;
  CATALA_INT length_field;
} array_2_struct;

enum bar_enum_code {
  bar_enum_no_cons,
  bar_enum_yes_cons
} bar_enum_code;

typedef struct bar_enum {
  enum bar_enum_code code;
  union {
    CATALA_UNIT no_cons;
    foo_struct yes_cons;
  } payload;
} bar_enum;

typedef struct baz_struct {
  CATALA_DEC b_field;
  array_3_struct c_field;
} baz_struct;

enum option_3_enum_code {
  option_3_enum_none_3_cons,
  option_3_enum_some_3_cons
} option_3_enum_code;

typedef struct option_3_enum {
  enum option_3_enum_code code;
  union {
    CATALA_UNIT none_3_cons;
    array_3_struct some_3_cons;
  } payload;
} option_3_enum;

enum option_1_enum_code {
  option_1_enum_none_1_cons,
  option_1_enum_some_1_cons
} option_1_enum_code;

typedef struct option_1_enum {
  enum option_1_enum_code code;
  union {
    CATALA_UNIT none_1_cons;
    bar_enum some_1_cons;
  } payload;
} option_1_enum;

typedef struct array_4_struct {
  option_3_enum * content_field;
  CATALA_INT length_field;
} array_4_struct;

typedef struct array_1_struct {
  option_1_enum * content_field;
  CATALA_INT length_field;
} array_1_struct;

typedef struct tuple_1_struct {
  option_1_enum (*elt_0_field)(void * /* closure_env */ arg_0_typ, CATALA_UNIT arg_1_typ);
  void * /* closure_env */ elt_1_field;
} tuple_1_struct;

typedef struct baz_in_struct {
  tuple_1_struct a_in_field;
} baz_in_struct;

baz_struct baz_func(baz_in_struct baz_in) {
  tuple_1_struct a;
  bar_enum temp_a;
  option_1_enum temp_a_1;
  array_1_struct temp_a_2;
  tuple_1_struct code_and_env;
  option_1_enum (*code)(void * /* closure_env */ arg_0_typ, CATALA_UNIT arg_1_typ);
  void * /* closure_env */ env;
  bar_enum a_1;
  CATALA_DEC temp_b;
  option_2_enum temp_b_1;
  array_2_struct temp_b_2;
  option_2_enum temp_b_3;
  array_2_struct temp_b_4;
  option_2_enum temp_b_5;
  array_2_struct temp_b_6;
  CATALA_DEC b;
  array_3_struct temp_c;
  option_3_enum temp_c_1;
  array_4_struct temp_c_2;
  option_3_enum temp_c_3;
  array_4_struct temp_c_4;
  array_3_struct c;
  baz_struct baz;
  a = baz_in.a_in_field;
  code_and_env = a;
  code = code_and_env.elt_0_field;
  env = code_and_env.elt_1_field;
  temp_a_2.content_field = catala_malloc(sizeof(array_1_struct));
  temp_a_2.content_field[0] = code(env, CATALA_UNITVAL);
  option_1_enum match_arg = handle_exceptions(temp_a_2);
  switch (match_arg.code) {
    case option_1_enum_none_1_cons:
      if (CATALA_TRUE) {
        bar_enum temp_a_3;
        option_1_enum temp_a_4;
        array_1_struct temp_a_5;
        option_1_enum temp_a_6;
        array_1_struct temp_a_7;
        option_1_enum temp_a_1;
        temp_a_7.content_field = catala_malloc(sizeof(array_1_struct));
        
        option_1_enum match_arg_1 = handle_exceptions(temp_a_7);
        switch (match_arg_1.code) {
          case option_1_enum_none_1_cons:
            if (CATALA_TRUE) {
              bar_enum temp_a_8;
              option_1_enum temp_a_6;
              temp_a_8 = {bar_enum_no_cons, {no_cons: CATALA_UNITVAL}};
              temp_a_6 = {option_1_enum_some_1_cons,
                {some_1_cons: temp_a_8}};
            } else {
              option_1_enum temp_a_6;
              temp_a_6 = {option_1_enum_none_1_cons,
                {none_1_cons: CATALA_UNITVAL}};
            }
            break;
          case option_1_enum_some_1_cons:
            bar_enum x = match_arg_1.payload.some_1_cons;
            option_1_enum temp_a_6;
            temp_a_6 = {option_1_enum_some_1_cons, {some_1_cons: x}};
            break;
        }
        temp_a_5.content_field = catala_malloc(sizeof(array_1_struct));
        temp_a_5.content_field[0] = temp_a_6;
        option_1_enum match_arg_2 = handle_exceptions(temp_a_5);
        switch (match_arg_2.code) {
          case option_1_enum_none_1_cons:
            if (CATALA_FALSE) {
              option_1_enum temp_a_4;
              temp_a_4 = {option_1_enum_none_1_cons,
                {none_1_cons: CATALA_UNITVAL}};
            } else {
              option_1_enum temp_a_4;
              temp_a_4 = {option_1_enum_none_1_cons,
                {none_1_cons: CATALA_UNITVAL}};
            }
            break;
          case option_1_enum_some_1_cons:
            bar_enum x_1 = match_arg_2.payload.some_1_cons;
            option_1_enum temp_a_4;
            temp_a_4 = {option_1_enum_some_1_cons, {some_1_cons: x_1}};
            break;
        }
        option_1_enum match_arg_3 = temp_a_4;
        switch (match_arg_3.code) {
          case option_1_enum_none_1_cons:
            catala_raise(catala_no_value, tests/backends/simple.catala_en,
              11, 11, 11, 12);
            break;
          case option_1_enum_some_1_cons:
            bar_enum arg = match_arg_3.payload.some_1_cons;
            temp_a_3 = arg;
            break;
        }
        temp_a_1 = {option_1_enum_some_1_cons, {some_1_cons: temp_a_3}};
      } else {
        option_1_enum temp_a_1;
        temp_a_1 = {option_1_enum_none_1_cons,
          {none_1_cons: CATALA_UNITVAL}};
      }
      break;
    case option_1_enum_some_1_cons:
      bar_enum x_2 = match_arg.payload.some_1_cons;
      option_1_enum temp_a_1;
      temp_a_1 = {option_1_enum_some_1_cons, {some_1_cons: x_2}};
      break;
  }
  option_1_enum match_arg_4 = temp_a_1;
  switch (match_arg_4.code) {
    case option_1_enum_none_1_cons:
      catala_raise(catala_no_value, tests/backends/simple.catala_en, 11,
        11, 11, 12);
      break;
    case option_1_enum_some_1_cons:
      bar_enum arg_1 = match_arg_4.payload.some_1_cons;
      temp_a = arg_1;
      break;
  }
  a_1 = temp_a;
  temp_b_6.content_field = catala_malloc(sizeof(array_2_struct));
  
  option_2_enum match_arg_5 = handle_exceptions(temp_b_6);
  switch (match_arg_5.code) {
    case option_2_enum_none_2_cons:
      CATALA_BOOL temp_b_7;
      bar_enum match_arg_6 = a_1;
      switch (match_arg_6.code) {
        case bar_enum_no_cons: temp_b_7 = CATALA_TRUE; break;
        case bar_enum_yes_cons:
          foo_struct dummy_var = match_arg_6.payload.yes_cons;
          temp_b_7 = CATALA_FALSE;
          break;
      }
      if (temp_b_7) {
        option_2_enum temp_b_5;
        temp_b_5 = {option_2_enum_some_2_cons,
          {some_2_cons: catala_new_dec_str("42")}};
      } else {
        option_2_enum temp_b_5;
        temp_b_5 = {option_2_enum_none_2_cons,
          {none_2_cons: CATALA_UNITVAL}};
      }
      break;
    case option_2_enum_some_2_cons:
      CATALA_DEC x_3 = match_arg_5.payload.some_2_cons;
      option_2_enum temp_b_5;
      temp_b_5 = {option_2_enum_some_2_cons, {some_2_cons: x_3}};
      break;
  }
  temp_b_4.content_field = catala_malloc(sizeof(array_2_struct));
  temp_b_4.content_field[0] = temp_b_5;
  option_2_enum match_arg_7 = handle_exceptions(temp_b_4);
  switch (match_arg_7.code) {
    case option_2_enum_none_2_cons:
      if (CATALA_FALSE) {
        option_2_enum temp_b_3;
        temp_b_3 = {option_2_enum_none_2_cons,
          {none_2_cons: CATALA_UNITVAL}};
      } else {
        option_2_enum temp_b_3;
        temp_b_3 = {option_2_enum_none_2_cons,
          {none_2_cons: CATALA_UNITVAL}};
      }
      break;
    case option_2_enum_some_2_cons:
      CATALA_DEC x_4 = match_arg_7.payload.some_2_cons;
      option_2_enum temp_b_3;
      temp_b_3 = {option_2_enum_some_2_cons, {some_2_cons: x_4}};
      break;
  }
  temp_b_2.content_field = catala_malloc(sizeof(array_2_struct));
  temp_b_2.content_field[0] = temp_b_3;
  option_2_enum match_arg_8 = handle_exceptions(temp_b_2);
  switch (match_arg_8.code) {
    case option_2_enum_none_2_cons:
      if (CATALA_TRUE) {
        array_2_struct temp_b_8;
        option_2_enum temp_b_9;
        array_2_struct temp_b_10;
        temp_b_10.content_field = catala_malloc(sizeof(array_2_struct));
        
        option_2_enum match_arg_9 = handle_exceptions(temp_b_10);
        switch (match_arg_9.code) {
          case option_2_enum_none_2_cons:
            if (CATALA_TRUE) {
              CATALA_DEC temp_b_11;
              option_2_enum temp_b_9;
              bar_enum match_arg_10 = a_1;
              switch (match_arg_10.code) {
                case bar_enum_no_cons:
                  temp_b_11 = catala_new_dec_str("0");
                  break;
                case bar_enum_yes_cons:
                  foo_struct foo = match_arg_10.payload.yes_cons;
                  CATALA_DEC temp_b_12;
                  if (foo.x_field) {
                    temp_b_12 = catala_new_dec_str("1");
                  } else {
                    temp_b_12 = catala_new_dec_str("0");
                  }
                  temp_b_11 = o_add_rat_rat(foo.y_field, temp_b_12);
                  break;
              }
              temp_b_9 = {option_2_enum_some_2_cons,
                {some_2_cons: temp_b_11}};
            } else {
              option_2_enum temp_b_9;
              temp_b_9 = {option_2_enum_none_2_cons,
                {none_2_cons: CATALA_UNITVAL}};
            }
            break;
          case option_2_enum_some_2_cons:
            CATALA_DEC x_5 = match_arg_9.payload.some_2_cons;
            option_2_enum temp_b_9;
            temp_b_9 = {option_2_enum_some_2_cons, {some_2_cons: x_5}};
            break;
        }
        temp_b_8.content_field = catala_malloc(sizeof(array_2_struct));
        temp_b_8.content_field[0] = temp_b_9;
        option_2_enum match_arg_11 = handle_exceptions(temp_b_8);
        switch (match_arg_11.code) {
          case option_2_enum_none_2_cons:
            if (CATALA_FALSE) {
              option_2_enum temp_b_1;
              temp_b_1 = {option_2_enum_none_2_cons,
                {none_2_cons: CATALA_UNITVAL}};
            } else {
              option_2_enum temp_b_1;
              temp_b_1 = {option_2_enum_none_2_cons,
                {none_2_cons: CATALA_UNITVAL}};
            }
            break;
          case option_2_enum_some_2_cons:
            CATALA_DEC x_6 = match_arg_11.payload.some_2_cons;
            option_2_enum temp_b_1;
            temp_b_1 = {option_2_enum_some_2_cons, {some_2_cons: x_6}};
            break;
        }
      } else {
        option_2_enum temp_b_1;
        temp_b_1 = {option_2_enum_none_2_cons,
          {none_2_cons: CATALA_UNITVAL}};
      }
      break;
    case option_2_enum_some_2_cons:
      CATALA_DEC x_7 = match_arg_8.payload.some_2_cons;
      option_2_enum temp_b_1;
      temp_b_1 = {option_2_enum_some_2_cons, {some_2_cons: x_7}};
      break;
  }
  option_2_enum match_arg_12 = temp_b_1;
  switch (match_arg_12.code) {
    case option_2_enum_none_2_cons:
      catala_raise(catala_no_value, tests/backends/simple.catala_en, 12,
        10, 12, 11);
      break;
    case option_2_enum_some_2_cons:
      CATALA_DEC arg_2 = match_arg_12.payload.some_2_cons;
      temp_b = arg_2;
      break;
  }
  b = temp_b;
  temp_c_4.content_field = catala_malloc(sizeof(array_4_struct));
  
  option_3_enum match_arg_13 = handle_exceptions(temp_c_4);
  switch (match_arg_13.code) {
    case option_3_enum_none_3_cons:
      if (CATALA_TRUE) {
        array_3_struct temp_c_5;
        option_3_enum temp_c_3;
        temp_c_5.content_field = catala_malloc(sizeof(array_3_struct));
        temp_c_5.content_field[0] = b;
        temp_c_5.content_field[1] = b;
        temp_c_3 = {option_3_enum_some_3_cons, {some_3_cons: temp_c_5}};
      } else {
        option_3_enum temp_c_3;
        temp_c_3 = {option_3_enum_none_3_cons,
          {none_3_cons: CATALA_UNITVAL}};
      }
      break;
    case option_3_enum_some_3_cons:
      array_3_struct x_8 = match_arg_13.payload.some_3_cons;
      option_3_enum temp_c_3;
      temp_c_3 = {option_3_enum_some_3_cons, {some_3_cons: x_8}};
      break;
  }
  temp_c_2.content_field = catala_malloc(sizeof(array_4_struct));
  temp_c_2.content_field[0] = temp_c_3;
  option_3_enum match_arg_14 = handle_exceptions(temp_c_2);
  switch (match_arg_14.code) {
    case option_3_enum_none_3_cons:
      if (CATALA_FALSE) {
        option_3_enum temp_c_1;
        temp_c_1 = {option_3_enum_none_3_cons,
          {none_3_cons: CATALA_UNITVAL}};
      } else {
        option_3_enum temp_c_1;
        temp_c_1 = {option_3_enum_none_3_cons,
          {none_3_cons: CATALA_UNITVAL}};
      }
      break;
    case option_3_enum_some_3_cons:
      array_3_struct x_9 = match_arg_14.payload.some_3_cons;
      option_3_enum temp_c_1;
      temp_c_1 = {option_3_enum_some_3_cons, {some_3_cons: x_9}};
      break;
  }
  option_3_enum match_arg_15 = temp_c_1;
  switch (match_arg_15.code) {
    case option_3_enum_none_3_cons:
      catala_raise(catala_no_value, tests/backends/simple.catala_en, 13,
        10, 13, 11);
      break;
    case option_3_enum_some_3_cons:
      array_3_struct arg_3 = match_arg_15.payload.some_3_cons;
      temp_c = arg_3;
      break;
  }
  c = temp_c;
  baz = { b, c };
  return baz;
}

int main (int argc, char** argv) {
  catala_init();
}
