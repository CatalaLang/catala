/* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Louis Gesbert
   <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. */

#ifndef __CATALA_RUNTIME_H__
#define __CATALA_RUNTIME_H__

#include <gmp.h>
#include <dates_calc.h>

/* --- Error handling --- */

typedef enum catala_error_code
{
  catala_assertion_failed,
  catala_no_value,
  catala_conflict,
  catala_division_by_zero,
  catala_list_empty,
  catala_not_same_length,
  catala_uncomparable_durations,
  catala_ambiguous_date_rounding,
  catala_indivisible_durations,
  catala_malloc_error
} catala_error_code;

typedef struct catala_code_position
{
    const char *filename;
    unsigned int start_line;
    unsigned int start_column;
    unsigned int end_line;
    unsigned int end_column;
} catala_code_position;

struct catala_error
{
  const catala_code_position ** position;
  int nb_positions;
  catala_error_code code;
};

void catala_error(catala_error_code code,
                  const catala_code_position ** pos,
                  const int nb_pos);

/* --- Memory allocations --- */

void* catala_malloc (size_t sz);

void catala_free_all(void);

void catala_set_persistent_malloc(void);
void catala_unset_persistent_malloc(void);
/* These two functions can be used for switching an init section to persistent
   malloc, then switching back to catala built-in malloc. In other words, any
   calls to `catala_malloc` done between the two will not be affected by
   `catala_free_all()`. Calls can be nested, but errors reset the context. */
#define CATALA_GET_LAZY(X, X_INIT) (X ? X : (catala_set_persistent_malloc(), X = X_INIT, catala_unset_persistent_malloc(), X))

/* --- Base types --- */

#define CATALA_BOOL const int*
#define CATALA_UNIT const void*
#define CATALA_INT mpz_srcptr
#define CATALA_DEC mpq_srcptr
#define CATALA_MONEY mpz_srcptr
#define CATALA_DATE const dc_date*
#define CATALA_DURATION const dc_period*
#define CATALA_ARRAY(_) catala_array*
#define CATALA_POSITION const catala_code_position*

typedef struct tuple_element {
  const void* content;
} tuple_element;
#define CATALA_TUPLE(_) tuple_element*

#define CATALA_EXN(X) CATALA_OPTION(CATALA_TUPLE(X,CATALA_POSITION))

#define CLOSURE_ENV void**

typedef struct catala_closure {
  void (*funcp)(void);
  const CLOSURE_ENV env;
} catala_closure;

extern const int * const catala_true;
#define CATALA_TRUE catala_true

extern const int * const catala_false;
#define CATALA_FALSE catala_false

extern const int catala_unitval;
#define CATALA_UNITVAL &catala_unitval

typedef struct catala_array {
  size_t size;
  void const ** elements;
} catala_array;

enum catala_option_code {
  catala_option_none,
  catala_option_some
};

typedef struct catala_option {
  enum catala_option_code code;
  const void* payload;
} catala_option;

#define CATALA_OPTION(_) catala_option*

extern const catala_option catala_none;
#define CATALA_NONE &catala_none

const CATALA_OPTION(X) catala_some (const void* x);

CATALA_BOOL catala_isnone (const CATALA_OPTION() opt);

/* --- Constructors --- */

CATALA_BOOL catala_new_bool(const int);

CATALA_INT catala_new_int(const signed long int val);

/* Arg is a null-terminated string */
CATALA_INT catala_new_int_str(const char* val);

CATALA_DEC catala_new_dec (const signed long int units,
                    const unsigned long int decimals);

/* Arg is a null-terminated string that must be in fraction form (eg 1234/100,
   not 12.34) */
CATALA_DEC catala_new_dec_str(const char* val);

CATALA_INT catala_new_money(const signed long int val);

/* Arg is a null-terminated string */
CATALA_MONEY catala_new_money_str(const char* val);

CATALA_DATE catala_new_date(const signed long int year,
                            const unsigned long int month,
                            const unsigned long int day);

CATALA_DURATION catala_new_duration(const long int years,
                                    const long int months,
                                    const long int days);

/* --- Operators --- */

CATALA_BOOL o_not(CATALA_BOOL b);

CATALA_INT o_length(const CATALA_ARRAY() arr);

CATALA_INT o_getDay(CATALA_DATE date);

CATALA_INT o_getMonth(CATALA_DATE date);

CATALA_INT o_getYear(CATALA_DATE date);

CATALA_DATE o_firstDayOfMonth(CATALA_DATE date);

CATALA_DATE o_lastDayOfMonth(CATALA_DATE date);

CATALA_INT o_minus_int (CATALA_INT x);

CATALA_DEC o_minus_rat (CATALA_DEC x);

CATALA_MONEY o_minus_mon (CATALA_MONEY x);

CATALA_DURATION o_minus_dur (CATALA_DURATION dur);

CATALA_INT o_toint_rat (CATALA_DEC x);

CATALA_DEC o_torat_int (CATALA_INT x);

CATALA_DEC o_torat_mon (CATALA_MONEY x);

CATALA_MONEY o_tomoney_rat (CATALA_DEC x);

CATALA_DEC o_round_rat (CATALA_DEC x);

CATALA_MONEY o_round_mon (CATALA_MONEY x);

CATALA_BOOL o_and (CATALA_BOOL x1, CATALA_BOOL x2);

CATALA_BOOL o_or (CATALA_BOOL x1, CATALA_BOOL x2);

CATALA_BOOL o_xor (CATALA_BOOL x1, CATALA_BOOL x2);

CATALA_INT o_add_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_DEC o_add_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_MONEY o_add_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2);

CATALA_DATE o_add_dat_dur (dc_date_rounding mode,
                           const catala_code_position* pos,
                           CATALA_DATE x1,
                           CATALA_DURATION x2);

CATALA_DURATION o_add_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2);

CATALA_INT o_sub_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_DEC o_sub_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_MONEY o_sub_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2);

CATALA_DURATION o_sub_dat_dat (CATALA_DATE x1, CATALA_DATE x2);

CATALA_DATE o_sub_dat_dur (dc_date_rounding mode,
                           const catala_code_position* pos,
                           CATALA_DATE x1, CATALA_DURATION x2);

CATALA_DURATION o_sub_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2);

CATALA_INT o_mult_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_DEC o_mult_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_MONEY o_mult_mon_int (CATALA_MONEY x1, CATALA_INT x2);

CATALA_MONEY o_mult_mon_rat (CATALA_MONEY x1, CATALA_DEC x2);

CATALA_DURATION o_mult_dur_int (CATALA_DURATION x1, CATALA_INT x2);

CATALA_DEC o_div_int_int (const catala_code_position* pos,
                          CATALA_INT x1,
                          CATALA_INT x2);

CATALA_DEC o_div_rat_rat (const catala_code_position* pos,
                          CATALA_DEC x1,
                          CATALA_DEC x2);

CATALA_DEC o_div_mon_mon (const catala_code_position* pos,
                          CATALA_MONEY x1,
                          CATALA_MONEY x2);

CATALA_MONEY o_div_mon_int (const catala_code_position* pos,
                            CATALA_MONEY x1,
                            CATALA_INT x2);

CATALA_MONEY o_div_mon_rat (const catala_code_position* pos,
                            CATALA_MONEY x1,
                            CATALA_DEC x2);

CATALA_DEC o_div_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1,
                          CATALA_DURATION x2);

CATALA_BOOL o_eq_boo_boo (CATALA_BOOL x1, CATALA_BOOL x2);

CATALA_BOOL o_eq_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_BOOL o_eq_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_BOOL o_eq_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2);

CATALA_BOOL o_eq_dat_dat (CATALA_DATE x1, CATALA_DATE x2);

CATALA_BOOL o_eq_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2);

CATALA_BOOL o_lt_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_BOOL o_lt_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_BOOL o_lt_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2);

CATALA_BOOL o_lt_dat_dat (CATALA_DATE x1, CATALA_DATE x2);

CATALA_BOOL o_lt_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2);

CATALA_BOOL o_lte_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_BOOL o_lte_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_BOOL o_lte_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2);

CATALA_BOOL o_lte_dat_dat (CATALA_DATE x1, CATALA_DATE x2);

CATALA_BOOL o_lte_dur_dur (const catala_code_position* pos,
                           CATALA_DURATION x1, CATALA_DURATION x2);

CATALA_BOOL o_gt_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_BOOL o_gt_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_BOOL o_gt_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2);

CATALA_BOOL o_gt_dat_dat (CATALA_DATE x1, CATALA_DATE x2);

CATALA_BOOL o_gt_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2);

CATALA_BOOL o_gte_int_int (CATALA_INT x1, CATALA_INT x2);

CATALA_BOOL o_gte_rat_rat (CATALA_DEC x1, CATALA_DEC x2);

CATALA_BOOL o_gte_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2);

CATALA_BOOL o_gte_dat_dat (CATALA_DATE x1, CATALA_DATE x2);

CATALA_BOOL o_gte_dur_dur (const catala_code_position* pos,
                           CATALA_DURATION x1, CATALA_DURATION x2);

const CATALA_ARRAY(X) o_filter (catala_closure* cls, const CATALA_ARRAY(X) x);

const CATALA_ARRAY(Y) o_map (catala_closure* cls, const CATALA_ARRAY(X) x);

const void* o_fold (catala_closure* cls,
                    const void* init, const CATALA_ARRAY(X) x);

const void* o_reduce (catala_closure* cls,
                      catala_closure* dft, const CATALA_ARRAY(X) x);

const CATALA_ARRAY(Z) o_map2 (const catala_code_position* pos,
                        catala_closure* cls,
                        const CATALA_ARRAY(X) x,
                        const CATALA_ARRAY(Y) y);

const CATALA_ARRAY(Z) o_concat (const CATALA_ARRAY(X) x,
                                const CATALA_ARRAY(Y) y);

const CATALA_EXN(X) handle_exceptions
  (const CATALA_ARRAY(const CATALA_EXN(X)) e);

/* --- Runtime initialisation --- */

void register_error_handler(void (*f)(const struct catala_error *));

void catala_init(void);
/* This must be called once and before any use of the functions above: it
   performs necessary initialisations of GMP, as well as the setup for our error
   handling mechanism. */

#endif /* __CATALA_RUNTIME_H__ */
