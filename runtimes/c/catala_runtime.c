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


#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <gmp.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#include <dates_calc.h>
#include "catala_runtime.h"

__thread int catala_persistent_malloc_mode_on = 0;

/* --- Error handling --- */

__thread const catala_code_position catala_empty_position =
  { NULL, 0, 0, 0, 0 };

__thread struct catala_error catala_error_raised =
  { NULL, 0, 0, NULL };

__thread jmp_buf catala_error_jump_buffer;
__thread int has_jump_buffer = 0;

void catala_error(catala_error_code code,
                  const catala_code_position * pos,
                  const int npos,
                  const char * note)
{
  catala_error_raised.code = code;
  catala_error_raised.position = pos;
  catala_error_raised.nb_positions = npos;
  catala_error_raised.note = note;
  catala_persistent_malloc_mode_on = 0;
  if (has_jump_buffer)
    longjmp(catala_error_jump_buffer, 1);
  else {
    fprintf(stderr, "\033[1;31m[ERROR]\033[m Catala error triggered outside of 'catala_do'\n");
    abort();
  }
}

/* --- Memory allocations --- */

#define BLOCKSIZE 16384

struct catala_heap
{
  void* mem;    /* Start of the allocated block */
  void* curptr; /* Pointer to the first unattributed yet byte in mem */
  void* end;    /* End boundary of the allocated block (excluded) */
  struct catala_heap* next;
};

__thread struct catala_heap catala_heap = {NULL, NULL, NULL, NULL};

#define MALLOC_CHECK(PTR)                                   \
  if (PTR == NULL) {                                        \
    catala_error_raised.code = catala_malloc_error;   \
    longjmp(catala_error_jump_buffer, 1);             \
  }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpointer-arith"

void* catala_malloc (size_t sz)
{
  void* ptr = catala_heap.curptr;
  void* nextptr = ptr + sz;
  if (catala_persistent_malloc_mode_on) {
    return malloc(sz);
  } else if (nextptr < catala_heap.end) {
    catala_heap.curptr = nextptr;
    return ptr;
  } else {
    size_t alloc_size =
      BLOCKSIZE * ((sz + BLOCKSIZE - 1) / BLOCKSIZE);
    void* mem = calloc(alloc_size, 1);
    struct catala_heap* next = malloc(sizeof(catala_heap));
    MALLOC_CHECK(mem);
    MALLOC_CHECK(next);
    *next = catala_heap;
    catala_heap.mem = mem;
    catala_heap.curptr = mem + sz;
    catala_heap.end = mem + alloc_size;
    catala_heap.next = next;
    return mem;
  }
}

void catala_free_all(void)
{
  struct catala_heap * next_ptr;
  while (catala_heap.mem != NULL) {
    free(catala_heap.mem);
    next_ptr = catala_heap.next;
    catala_heap = *catala_heap.next;
    free(next_ptr);
  }
}

void* catala_realloc(void* oldptr, size_t oldsize, size_t newsize)
{
  if (catala_persistent_malloc_mode_on) {
    return realloc(oldptr, newsize);
  } else if (newsize <= oldsize) {
    memset(oldptr + newsize, 0, oldsize - newsize);
    return oldptr;
  } else {
    void* ptr = catala_malloc(newsize);
    memcpy(ptr, oldptr, oldsize);
    return ptr;
  }
}

void catala_free(void* ptr, size_t sz)
{
  /* All pointers are freed in bulk by catala_free_all */
  return;
}
#pragma GCC diagnostic pop

void catala_set_persistent_malloc(void) {
  catala_persistent_malloc_mode_on++;
}
void catala_unset_persistent_malloc(void) {
  assert (catala_persistent_malloc_mode_on > 0);
  catala_persistent_malloc_mode_on--;
}

/* --- Base types --- */

const int catala_true_value = 1;
const int * const catala_true = &catala_true_value;
const int catala_false_value = 0;
const int * const catala_false = &catala_false_value;
const int catala_unitval = 0;

/* --- Constructors --- */

static mpz_t zconst_100;

#define CATALA_NEW_BOOL(X) \
  ((X) ? CATALA_TRUE : CATALA_FALSE)

#define CATALA_NEW_MPZ(X) \
  const mpz_ptr X = catala_malloc(sizeof(__mpz_struct)); \
  mpz_init(X)

#define CATALA_NEW_MPQ(X) \
  const mpq_ptr X = catala_malloc(sizeof(__mpq_struct)); \
  mpq_init(X)

CATALA_BOOL catala_new_bool(const int x)
{
  return CATALA_NEW_BOOL(x);
}

CATALA_INT catala_new_int(const signed long int val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_si(ret, val);
  return ret;
}

/* Arg is a null-terminated string */
CATALA_INT catala_new_int_str(const char* val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_str(ret, val, 10);
  return ret;
}

CATALA_DEC catala_new_frac (const signed long int num,
                            const unsigned long int den)
{
  CATALA_NEW_MPQ(ret);
  mpz_set_si(mpq_numref(ret), num);
  mpz_set_ui(mpq_denref(ret), den);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_DEC catala_new_dec (const signed long int units,
                    const unsigned long int decimals)
{
  mpz_t zdec;
  int dcount = decimals;
  CATALA_NEW_MPQ(ret);
  mpz_init_set_ui(zdec, decimals);
  mpz_set_si(mpq_numref(ret), units);
  while (dcount > 0) {
    mpz_mul_ui(mpq_denref(ret), mpq_denref(ret), 10);
    dcount /= 10;
  }
  mpz_mul(mpq_numref(ret), mpq_numref(ret), mpq_denref(ret));
  mpz_add(mpq_numref(ret), mpq_numref(ret), zdec);
  mpz_clear(zdec);
  mpq_canonicalize(ret);
  return ret;
}

/* Arg is a null-terminated string that must be in fraction form (eg 1234/100,
   not 12.34) */
CATALA_DEC catala_new_dec_str(const char* val)
{
  CATALA_NEW_MPQ(ret);
  mpq_set_str(ret, val, 10);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_MONEY catala_new_money(const signed long int val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_si(ret, val);
  return ret;
}

/* Arg is a null-terminated string */
CATALA_MONEY catala_new_money_str(const char* val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_str(ret, val, 10);
  return ret;
}

CATALA_DATE catala_new_date(const signed long int year,
                            const unsigned long int month,
                            const unsigned long int day)
{
  dc_date *ret = catala_malloc(sizeof(dc_date));
  int success = dc_make_date(ret, year, month, day);
  /* This could fail, but is expected to only be called with known, already
     validated arguments by the generated code */
  assert(success == dc_ok);
  return ret;
}

CATALA_DURATION catala_new_duration(const long int years,
                                    const long int months,
                                    const long int days)
{
  dc_period* ret = catala_malloc(sizeof(dc_period));
  dc_make_period(ret, years, months, days);
  return ret;
}

void catala_set_array(catala_array* ret, const int size, ...)
{
  int i;
  va_list args;
  ret->size = size;
  if (size > 0)
    ret->elements = catala_malloc(size * sizeof(void*));
  va_start (args, size);
  for (i = 0; i < size; i++)
    ret->elements[i] = va_arg(args, void*);
  va_end(args);
  return;
}

CATALA_ARRAY(X) catala_new_array(const int size, ...)
{
  int i;
  va_list args;
  catala_array* ret = catala_malloc(sizeof(catala_array));
  ret->size = size;
  if (size > 0)
    ret->elements = catala_malloc(size * sizeof(void*));
  va_start (args, size);
  for (i = 0; i < size; i++)
    ret->elements[i] = va_arg(args, void*);
  va_end(args);
  return ret;
}

CATALA_TUPLE(_) catala_new_tuple(const int size, ...)
{
  int i;
  va_list args;
  tuple_element* ret =
    catala_malloc(size * sizeof(tuple_element));
  va_start (args, size);
  for (i = 0; i < size; i++)
    ret[i].content = va_arg(args, void*);
  va_end(args);
  return ret;
}

/* --- Value embedding --- */

catala_value embed (const catala_type t, const void* v) {
  catala_value ret;
  ret.t = t;
  ret.v = v;
  return ret;
}

int catala_equal_durations(const catala_code_position* pos,
                           CATALA_DURATION x1, CATALA_DURATION x2) {
  long int y1 = dc_period_years(x1);
  long int m1 = dc_period_months(x1);
  long int d1 = dc_period_days(x1);
  long int y2 = dc_period_years(x2);
  long int m2 = dc_period_months(x2);
  long int d2 = dc_period_days(x2);
  if (y1 == y2 && m1 == m2 && d1 == d2)
    return 1;
  else if (d1 == 0 && d2 == 0)
    return (y1 * 12 + m1 == y2 * 12 + m2);
  else if (y1 == 0 && y2 == 0 && m1 == 0 && m2 == 0)
    return (d1 == d2);
  else {
    catala_error(catala_uncomparable_durations, pos, 1, NULL);
    abort();
  }
}

/* TODO: backport as dc_compare_periods to dates_calc ? */
int catala_compare_durations(const catala_code_position* pos,
                             CATALA_DURATION x1, CATALA_DURATION x2) {
  long int y1 = dc_period_years(x1);
  long int m1 = dc_period_months(x1);
  long int d1 = dc_period_days(x1);
  long int y2 = dc_period_years(x2);
  long int m2 = dc_period_months(x2);
  long int d2 = dc_period_days(x2);
  if (d1 == 0 && d2 == 0) {
    m1 = 12 * y1 + m1;
    m2 = 12 * y2 + m2;
    return m1 < m2 ? -1 : (m1 > m2);
  } else if (y1 == 0 && y2 == 0 && m1 == 0 && m2 == 0) {
    return d1 < d2 ? -1 : (d1 > d2);
  } else {
    catala_error(catala_uncomparable_durations, pos, 1, NULL);
  }
  abort();
}

int catala_equal_positions(const catala_code_position* x1,
                           const catala_code_position* x2) {
  return (strcmp(x1->filename, x2->filename) == 0
          && x1->start_line == x2->start_line
          && x1->end_line == x2->end_line
          && x1->start_column == x2->start_column
          && x1->end_column == x2->end_column);
}

#define COMPARE(X, Y) ((X) == (Y) ? 0 : (X) > (Y) ? 1 : -1)

int catala_compare_positions(const catala_code_position* x1,
                             const catala_code_position* x2) {
  int cmp = strcmp(x1->filename, x2->filename);
  if (cmp) return cmp;
  cmp = COMPARE(x1->start_line, x2->start_line);
  if (cmp) return cmp;
  cmp = COMPARE(x1->end_line, x2->end_line);
  if (cmp) return cmp;
  cmp = COMPARE(x1->start_column, x2->start_column);
  if (cmp) return cmp;
  cmp = COMPARE(x1->end_column, x2->end_column);
  return cmp;
}

/* generic pattern for the encoding of catala enums */
struct catala_enum {
  unsigned int code; /* this is actually a C enum; from what I could read,
                        casting to unsigned int is a reasonable assumption, but
                        there is still a possibility it breaks on exotic
                        compilers. (Even if smaller, the struct should pad it
                        for alignemnt so it mays till work) */
  void* payload;
};

int catala_equal_values (const catala_code_position* pos, const catala_value x1, const catala_value x2);
int catala_equal (const catala_type ty, const catala_code_position* pos, const void* x1, const void* x2) {
  switch (ty.kind) {
  case UNIT: return 1;
  case BOOL: return (!*(CATALA_BOOL)x1 == !*(CATALA_BOOL)x2);
  case INTEGER: return (mpz_cmp(x1, x2) == 0);
  case DECIMAL: return (mpq_equal(x1, x2));
  case MONEY: return (mpz_cmp(x1, x2) == 0);
  case DATE: return (dc_compare_dates(x1, x2) == 0);
  case DURATION: return catala_equal_durations(pos, x1, x2);
  case POSITION: return catala_equal_positions(x1, x2);
  default: return catala_equal_values(pos, embed(ty,x1), embed(ty,x2));
  }
}
int catala_equal_values (const catala_code_position* pos, const catala_value x1, const catala_value x2) {
  int i;
  if (x1.t.kind != x2.t.kind) return 0;
  switch (x1.t.kind) {
  case ARRAY: {
    const CATALA_ARRAY() a1 = x1.v;
    const CATALA_ARRAY() a2 = x2.v;
    if (a1->size != a2->size) return 0;
    for (i = 0; i < a1->size; i++)
      if (!catala_equal_values (pos,
                                embed(*x1.t.contents.tarray, a1->elements[i]),
                                embed(*x2.t.contents.tarray, a2->elements[i])))
        return 0;
    return 1;
  }
  case TUPLE: {
    const CATALA_TUPLE() a1 = x1.v;
    const CATALA_TUPLE() a2 = x2.v;
    if (x1.t.contents.ttuple.size != x2.t.contents.ttuple.size) return 0; /* Typing error */
    for (i = 0; i < x1.t.contents.ttuple.size; i++) {
      const catala_type * t1 = x1.t.contents.ttuple.elements[i];
      const catala_type * t2 = x2.t.contents.ttuple.elements[i];
      if (!catala_equal_values
          (pos,
           embed(*t1, a1[i].content),
           embed(*t2, a2[i].content)))
        return 0;
    }
    return 1;
  }
  case STRUCT: {
    /* Our struct contain only Catala values which are pointers.
       So we assume that a struct can be cast to an array of pointers. */
    const void* const* v1 = x1.v;
    const void* const* v2 = x2.v;
    if (strcmp(x1.t.contents.tstruct.name, x2.t.contents.tstruct.name)
        || x1.t.contents.tstruct.size != x2.t.contents.tstruct.size)
      return 0; /* Typing error */
    for (i = 0; i < x1.t.contents.tstruct.size; i++) {
      if (!catala_equal_values
          (pos,
           embed(x1.t.contents.tstruct.fields[i].ty, v1[i]),
           embed(x2.t.contents.tstruct.fields[i].ty, v2[i])))
        return 0;
    }
    return 1;
  }
  case ENUM: {
    const struct catala_enum * e1 = x1.v;
    const struct catala_enum * e2 = x2.v;
    if (strcmp(x1.t.contents.tenum.name, x2.t.contents.tenum.name))
      return 0; /* Typing error */
    return (e1->code == e2->code &&
            catala_equal_values
            (pos,
             embed(x1.t.contents.tenum.cases[e1->code].ty, e1->payload),
             embed(x2.t.contents.tenum.cases[e2->code].ty, e2->payload)));
  }
  case EXTERNAL: {
    if (strcmp(x1.t.contents.texternal.name, x2.t.contents.texternal.name))
      return 0;
    return x1.t.contents.texternal.equal(pos, x1.v, x2.v);
  }
  case FUNCTION: {
    catala_error(catala_uncomparable_durations, pos, 1, NULL);
    abort();
  }
  default:
    return catala_equal(x1.t, pos, x1.v, x2.v);
  }
}

int catala_compare_values (const catala_code_position* pos, const catala_value x1, const catala_value x2);
int catala_compare (const catala_type ty, const catala_code_position* pos, const void* x1, const void* x2) {
  switch (ty.kind) {
  case UNIT: return 0;
  case BOOL:
    return *(CATALA_BOOL)x1 ? (*(CATALA_BOOL)x2 ?  0 : 1) :
                              (*(CATALA_BOOL)x2 ? -1 : 0);
  case INTEGER: return (mpz_cmp(x1, x2));
  case DECIMAL: return (mpq_cmp(x1, x2));
  case MONEY: return (mpz_cmp(x1, x2));
  case DATE: return (dc_compare_dates(x1, x2));
  case DURATION: return catala_compare_durations(pos, x1, x2);
  case POSITION: return catala_compare_positions(x1, x2);
  default: return catala_compare_values(pos, embed(ty,x1), embed(ty,x2));
  }
}
#define MIN(X,Y) ((X) <= (Y) ? (X) : (Y))
int catala_compare_values (const catala_code_position* pos, const catala_value x1, const catala_value x2) {
  int i;
  if (x1.t.kind != x2.t.kind) return (x1.t.kind - x2.t.kind);
  switch (x1.t.kind) {
  case ARRAY: {
    const CATALA_ARRAY() a1 = x1.v;
    const CATALA_ARRAY() a2 = x2.v;
    for (i = 0; i < MIN(a1->size, a2->size); i++) {
      int cmp = catala_compare_values(pos,
          embed(*x1.t.contents.tarray, a1->elements[i]),
          embed(*x2.t.contents.tarray, a2->elements[i]));
      if (cmp) return cmp;
    }
    return COMPARE(a1->size, a2->size);
  }
  case TUPLE: {
    const CATALA_TUPLE() a1 = x1.v;
    const CATALA_TUPLE() a2 = x2.v;
    int cmp = COMPARE(x1.t.contents.ttuple.size, x2.t.contents.ttuple.size);
    if (cmp) return cmp;
    for (i = 0; i < x1.t.contents.ttuple.size; i++) {
      const catala_type * t1 = x1.t.contents.ttuple.elements[i];
      const catala_type * t2 = x2.t.contents.ttuple.elements[i];
      cmp = catala_compare_values (pos,
          embed(*t1, a1[i].content),
          embed(*t2, a2[i].content));
      if (cmp) return cmp;
    }
    return 0;
  }
  case STRUCT: {
    /* Our struct contain only Catala values which are pointers.
       So we assume that a struct can be cast to an array of pointers. */
    const void* const* v1 = x1.v;
    const void* const* v2 = x2.v;
    int cmp = strcmp(x1.t.contents.tstruct.name, x2.t.contents.tstruct.name);
    if (cmp) return cmp;
    cmp = COMPARE(x1.t.contents.tstruct.size, x2.t.contents.tstruct.size);
    if (cmp) return cmp;
    for (i = 0; i < x1.t.contents.tstruct.size; i++) {
      cmp = catala_compare_values (pos,
          embed(x1.t.contents.tstruct.fields[i].ty, v1[i]),
          embed(x2.t.contents.tstruct.fields[i].ty, v2[i]));
      if (cmp) return cmp;
    }
    return 0;
  }
  case ENUM: {
    const struct catala_enum * e1 = x1.v;
    const struct catala_enum * e2 = x2.v;
    int cmp = strcmp(x1.t.contents.tenum.name, x2.t.contents.tenum.name);
    if (cmp) return cmp;
    cmp = COMPARE(e1->code, e2->code);
    if (cmp) return cmp;
    return (catala_compare_values
            (pos,
             embed(x1.t.contents.tenum.cases[e1->code].ty, e1->payload),
             embed(x2.t.contents.tenum.cases[e2->code].ty, e2->payload)));
  }
  case EXTERNAL: {
    int cmp = strcmp(x1.t.contents.texternal.name, x2.t.contents.texternal.name);
    if (cmp) return cmp;
    return x1.t.contents.texternal.compare(pos, x1.v, x2.v);
  }
  case FUNCTION: {
    catala_error(catala_uncomparable_durations, pos, 1, NULL);
    abort();
  }
  default:
    return catala_compare(x1.t, pos, x1.v, x2.v);
  }
}

void stdflush() {
  puts("");
  fflush(stdout);
}
void stdprintf(const char * fmt, ...) {
  va_list args;
  va_start(args, fmt);
  gmp_vprintf(fmt, args);
  va_end(args);
}
const struct catala_buf catala_stdbuf = { &stdprintf, 0, &stdflush };

void catala_print (struct catala_buf buf, const catala_value x) {
  int i;
  switch (x.t.kind) {
  case UNINITIALIZED:
    buf.printf("???"); return;
  case UNIT:
    buf.printf("()"); return;
  case BOOL:
    buf.printf("%s", *(CATALA_BOOL)x.v ? "true" : "false"); return;
  case INTEGER:
    buf.printf("%Zd", x.v); return;
  case MONEY: {
    mpz_t units, cents;
    mpz_init(units), mpz_init(cents);
    mpz_tdiv_qr(units, cents, x.v, zconst_100);
    mpz_abs(cents, cents);
    buf.printf("%Zd.%02Zd€", units, cents);
    return;
  }
  case DECIMAL: {
    /* Note: this may do rounding, check the OCaml code for proper printing */
    mpf_t f;
    mpf_init(f);
    mpf_set_q(f, x.v);
    buf.printf("%01.10Ff", f);
    return;
  }
  case DATE:
    buf.printf("|%04d-%02d-%02d|", dc_date_year(x.v), dc_date_month(x.v), dc_date_day(x.v));
    return;
  case DURATION:
    buf.printf("[%d years, %d months, %d days]", dc_period_years(x.v), dc_period_months(x.v), dc_period_days(x.v));
    return;
  case POSITION: {
    CATALA_POSITION pos = x.v;
    buf.printf("%s:%d.%d-%d.%d",
               pos->filename,
               pos->start_line,
               pos->start_column,
               pos->end_line,
               pos->end_column);
    return;
  }
  case ARRAY: {
    const catala_array * a = x.v;
    buf.printf("[");
    buf.indent += 2;
    for (i = 0; i < a->size; i++) {
      buf.printf("\n% *s", buf.indent, "");
      catala_print(buf, embed(*x.t.contents.tarray, a->elements[i]));
      buf.printf(";");
    }
    buf.indent -= 2;
    if (i == 0) buf.printf("]");
    else buf.printf("\n% *s]", buf.indent, "");
    return;
  }
  case TUPLE: {
    const CATALA_TUPLE() a = x.v;
    buf.printf("(");
    buf.indent += 1;
    for (i = 0; i < x.t.contents.ttuple.size; i++) {
      const catala_type * t = x.t.contents.ttuple.elements[i];
      if (i > 0) buf.printf(", ");
      catala_print(buf, embed(*t, a[i].content));
    }
    buf.indent -= 1;
    buf.printf(")");
    return;
  }
  case STRUCT: {
    const void* const* a = x.v;
    buf.printf("%s {", x.t.contents.tstruct.name);
    buf.indent += 2;
    for (i = 0; i < x.t.contents.tstruct.size; i++) {
      buf.printf("\n% *s-- %s: ", buf.indent, "", x.t.contents.tstruct.fields[i].name);
      catala_print(buf, embed(x.t.contents.tstruct.fields[i].ty, a[i]));
    }
    buf.indent -= 2;
    if (i == 0) buf.printf("}");
    else buf.printf("\n% *s}", buf.indent, "");
    return;
  }
  case ENUM: {
    const struct catala_enum * e = x.v;
    const struct catala_label_type t = x.t.contents.tenum.cases[e->code];
    buf.printf("%s", t.name);
    if (t.ty.kind != UNIT) {
      buf.printf(" content ");
      buf.indent +=2;
      catala_print(buf, embed(t.ty, e->payload));
      buf.indent -=2;
    }
    return;
  }
  case EXTERNAL:
    x.t.contents.texternal.print(buf, x.v); return;
  case FUNCTION:
    buf.printf("<function>"); return;
  }
}
const char* catala_tojson (const catala_value val) {
  const char* ret = "\"todo\"";
  return ret;
}

/*   - base embedded types -    */

const catala_type catala_type_unit = {UNIT};
const catala_type catala_type_bool = {BOOL};
const catala_type catala_type_integer = {INTEGER};
const catala_type catala_type_decimal = {DECIMAL};
const catala_type catala_type_money = {MONEY};
const catala_type catala_type_date = {DATE};
const catala_type catala_type_duration = {DURATION};
const catala_type catala_type_position = {POSITION};
const catala_type catala_type_function = {FUNCTION};

const catala_type catala_type_array(const catala_type ty) {
  catala_type ret;
  ret.kind = ARRAY;
  ret.contents.tarray = catala_malloc(sizeof(catala_type));
  *ret.contents.tarray = ty;
  return ret;
}
const catala_type catala_type_tuple(int size, ...) {
  int i;
  va_list args;
  catala_type ret;
  ret.kind = TUPLE;
  ret.contents.ttuple.size = size;
  ret.contents.ttuple.elements = catala_malloc(size * sizeof(void*));
  va_start (args, size);
  for (i = 0; i < size; i++) {
    catala_type * ty = catala_malloc(sizeof (catala_type));
    *ty = va_arg(args, catala_type);
    ret.contents.ttuple.elements[i] = ty;
  }
  va_end(args);
  return ret;
}
const catala_type catala_type_struct
  (catala_type* ret,
   struct catala_label_type * fields,
   const char* name,
   int size, ...)
{
  int i;
  va_list args;
  ret->contents.tstruct.name = name;
  ret->contents.tstruct.size = size;
  ret->contents.tstruct.fields = fields;
  va_start (args, size);
  for (i = 0; i < size; i++) {
    fields[i].name = va_arg(args, char*);
    fields[i].ty = va_arg(args, catala_type);
  }
  va_end(args);
  ret->kind = STRUCT;
  return *ret;
}
const catala_type catala_type_enum
  (catala_type* ret,
   struct catala_label_type * cases,
   const char* name,
   int size, ...)
{
  int i;
  va_list args;
  ret->contents.tenum.name = name;
  ret->contents.tenum.size = size;
  ret->contents.tenum.cases = cases;
  va_start (args, size);
  for (i = 0; i < size; i++) {
    cases[i].name = va_arg(args, char*);
    cases[i].ty = va_arg(args, catala_type);
  }
  va_end(args);
  ret->kind = ENUM;
  return *ret;
}
const catala_type catala_type_undef = { UNINITIALIZED };
const catala_type catala_type_optional(const catala_type ty) {
  catala_type ret;
  ret.kind = ENUM;
  ret.contents.tenum.name = "Optional";
  ret.contents.tenum.size = 2;
  ret.contents.tenum.cases = catala_malloc(2*sizeof(struct catala_label_type));
  ret.contents.tenum.cases[0].name = "Absent";
  ret.contents.tenum.cases[0].ty = catala_type_unit;
  ret.contents.tenum.cases[1].name = "Present";
  ret.contents.tenum.cases[1].ty = ty;
  return ret;
}


/* --- Operators --- */

CATALA_BOOL o_not(CATALA_BOOL b)
{
  return CATALA_NEW_BOOL(! *b);
}

CATALA_INT o_length(const CATALA_ARRAY() arr)
{
  return catala_new_int(arr->size);
}

CATALA_INT o_getDay(CATALA_DATE date)
{
  return catala_new_int(dc_date_day(date));
}

CATALA_INT o_getMonth(CATALA_DATE date)
{
  return catala_new_int(dc_date_month(date));
}

CATALA_INT o_getYear(CATALA_DATE date)
{
  return catala_new_int(dc_date_year(date));
}

CATALA_DATE o_firstDayOfMonth(CATALA_DATE date)
{
  dc_date *ret = catala_malloc(sizeof(dc_date));
  dc_first_day_of_month(ret, date);
  return ret;
}

CATALA_DATE o_lastDayOfMonth(CATALA_DATE date)
{
  dc_date *ret = catala_malloc(sizeof(dc_date));
  dc_last_day_of_month(ret, date);
  return ret;
}

CATALA_INT o_minus_int (CATALA_INT x)
{
  CATALA_NEW_MPZ(ret);
  mpz_neg(ret, x);
  return ret;
}

CATALA_DEC o_minus_rat (CATALA_DEC x)
{
  CATALA_NEW_MPQ(ret);
  mpq_neg(ret, x);
  return ret;
}

CATALA_MONEY o_minus_mon (CATALA_MONEY x)
{
  CATALA_NEW_MPZ(ret);
  mpz_neg(ret, x);
  return ret;
}

CATALA_DURATION o_minus_dur (CATALA_DURATION dur)
{
  dc_period* ret = catala_malloc(sizeof(dc_period));
  dc_neg_period(ret, dur);
  return ret;
}

static void round_div(mpz_ptr ret, mpz_srcptr num, mpz_srcptr den)
{
  mpz_t den1;
  int sign;
  mpz_init(den1);
  sign = mpz_sgn(num) * mpz_sgn(den);
  mpz_abs(ret, num);
  mpz_abs(den1, den);
  /* if x = n/d, this is (2*n + d) / (2*d) */
  mpz_mul_ui(ret, ret, 2);
  mpz_add(ret, ret, den1);
  mpz_mul_ui(den1, den1, 2);
  /* round towards -inf */
  mpz_fdiv_q(ret, ret, den1);
  mpz_clear(den1);
  mpz_mul_si(ret, ret, sign);
  return;
}

CATALA_INT o_toint_rat (CATALA_DEC x)
{
  CATALA_NEW_MPZ(ret);
  mpz_tdiv_q(ret, mpq_numref(x), mpq_denref(x));
  return ret;
}

CATALA_INT o_toint_mon (CATALA_MONEY x)
{
  CATALA_NEW_MPZ(ret);
  round_div(ret, x, zconst_100);
  return ret;
}


CATALA_DEC o_torat_int (CATALA_INT x)
{
  CATALA_NEW_MPQ(ret);
  mpq_set_z(ret, x);
  return ret;
}

CATALA_DEC o_torat_mon (CATALA_MONEY x)
{
  CATALA_NEW_MPQ(ret);
  mpz_set(mpq_numref(ret), x);
  mpz_set_ui(mpq_denref(ret), 100);
  mpq_canonicalize(ret);
  return ret;
}



CATALA_MONEY o_tomoney_rat (CATALA_DEC x)
{
  CATALA_NEW_MPZ(ret);
  mpz_mul_ui(ret, mpq_numref(x), 100);
  round_div(ret, ret, mpq_denref(x));
  return ret;
}

CATALA_MONEY o_tomoney_int (CATALA_INT x)
{
  return o_tomoney_rat(o_torat_int(x));
}

CATALA_DEC o_round_rat (CATALA_DEC x)
{
  CATALA_NEW_MPQ(ret);
  round_div(mpq_numref(ret), mpq_numref(x), mpq_denref(x));
  return ret;
}

CATALA_MONEY o_round_mon (CATALA_MONEY x)
{
  CATALA_NEW_MPZ(ret);
  round_div(ret, x, zconst_100);
  mpz_mul_ui(ret, ret, 100);
  return ret;
}

CATALA_BOOL o_and (CATALA_BOOL x1, CATALA_BOOL x2)
{
  return CATALA_NEW_BOOL(*x1 && *x2);
}

CATALA_BOOL o_or (CATALA_BOOL x1, CATALA_BOOL x2)
{
  return CATALA_NEW_BOOL(*x1 || *x2);
}

CATALA_BOOL o_xor (CATALA_BOOL x1, CATALA_BOOL x2)
{
  return CATALA_NEW_BOOL(*x1 != *x2);
}

CATALA_INT o_add_int_int (CATALA_INT x1, CATALA_INT x2)
{
  CATALA_NEW_MPZ(ret);
  mpz_add(ret, x1, x2);
  return ret;
}

CATALA_DEC o_add_rat_rat (CATALA_DEC x1, CATALA_DEC x2)
{
  CATALA_NEW_MPQ(ret);
  mpq_add(ret, x1, x2);
  return ret;
}

CATALA_MONEY o_add_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2)
{
  CATALA_NEW_MPZ(ret);
  mpz_add(ret, x1, x2);
  return ret;
}

CATALA_DATE o_add_dat_dur (dc_date_rounding mode,
                           const catala_code_position* pos,
                           CATALA_DATE x1,
                           CATALA_DURATION x2)
{
  dc_date *ret = catala_malloc(sizeof(dc_date));
  if (dc_add_dates(ret, mode, x1, x2) != dc_ok)
    catala_error(catala_ambiguous_date_rounding, pos, 1, NULL);
  return ret;
}

CATALA_DURATION o_add_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2)
{
  dc_period *ret = catala_malloc(sizeof(dc_period));
  dc_add_periods(ret, x1, x2);
  return ret;
}

CATALA_INT o_sub_int_int (CATALA_INT x1, CATALA_INT x2)
{
  CATALA_NEW_MPZ(ret);
  mpz_sub(ret, x1, x2);
  return ret;
}

CATALA_DEC o_sub_rat_rat (CATALA_DEC x1, CATALA_DEC x2)
{
  CATALA_NEW_MPQ(ret);
  mpq_sub(ret, x1, x2);
  return ret;
}

CATALA_MONEY o_sub_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2)
{
  CATALA_NEW_MPZ(ret);
  mpz_sub(ret, x1, x2);
  return ret;
}

CATALA_DURATION o_sub_dat_dat (CATALA_DATE x1, CATALA_DATE x2)
{
  dc_period *ret = catala_malloc(sizeof(dc_period));
  dc_sub_dates(ret, x1, x2);
  return ret;
}

CATALA_DATE o_sub_dat_dur (dc_date_rounding mode,
                           const catala_code_position* pos,
                           CATALA_DATE x1, CATALA_DURATION x2)
{
  dc_period dur;
  dc_date *ret = catala_malloc(sizeof(dc_date));
  dc_neg_period(&dur, x2);
  if (dc_add_dates(ret, mode, x1, &dur) != dc_ok)
    catala_error(catala_ambiguous_date_rounding, pos, 1, NULL);
  return ret;
}

CATALA_DURATION o_sub_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2)
{
  dc_period *ret = catala_malloc(sizeof(dc_period));
  dc_sub_periods(ret, x1, x2);
  return ret;
}

CATALA_INT o_mult_int_int (CATALA_INT x1, CATALA_INT x2)
{
  CATALA_NEW_MPZ(ret);
  mpz_mul(ret, x1, x2);
  return ret;
}

CATALA_DEC o_mult_rat_rat (CATALA_DEC x1, CATALA_DEC x2)
{
  CATALA_NEW_MPQ(ret);
  mpq_mul(ret, x1, x2);
  return ret;
}

CATALA_MONEY o_mult_mon_int (CATALA_MONEY x1, CATALA_INT x2)
{
  CATALA_NEW_MPZ(ret);
  mpz_mul(ret, x1, x2);
  return ret;
}

CATALA_MONEY o_mult_mon_rat (CATALA_MONEY x1, CATALA_DEC x2)
{
  CATALA_NEW_MPZ(ret);
  mpz_mul(ret, x1, mpq_numref(x2));
  round_div(ret, ret, mpq_denref(x2));
  return ret;
}

CATALA_DURATION o_mult_dur_int (CATALA_DURATION x1, CATALA_INT x2)
{
  const signed long int mult = mpz_get_si(x2);
  dc_period *ret = catala_malloc(sizeof(dc_period));
  dc_mul_periods(ret, x1, mult);
  return ret;
}

CATALA_DEC o_div_int_int (const catala_code_position* pos,
                          CATALA_INT x1,
                          CATALA_INT x2)
{
  CATALA_NEW_MPQ(ret);
  if (mpz_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos, 1, NULL);
  mpz_set(mpq_numref(ret), x1);
  mpz_set(mpq_denref(ret), x2);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_DEC o_div_rat_rat (const catala_code_position* pos,
                          CATALA_DEC x1,
                          CATALA_DEC x2)
{
  CATALA_NEW_MPQ(ret);
  if (mpq_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos, 1, NULL);
  mpq_div(ret, x1, x2);
  return ret;
}

CATALA_DEC o_div_mon_mon (const catala_code_position* pos,
                          CATALA_MONEY x1,
                          CATALA_MONEY x2)
{
  CATALA_NEW_MPQ(ret);
  if (mpz_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos, 1, NULL);
  mpz_set(mpq_numref(ret), x1);
  mpz_set(mpq_denref(ret), x2);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_MONEY o_div_mon_int (const catala_code_position* pos,
                            CATALA_MONEY x1,
                            CATALA_INT x2)
{
  CATALA_NEW_MPZ(ret);
  if (mpz_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos, 1, NULL);
  round_div(ret, x1, x2);
  return ret;
}

CATALA_MONEY o_div_mon_rat (const catala_code_position* pos,
                            CATALA_MONEY x1,
                            CATALA_DEC x2)
{
  CATALA_NEW_MPZ(ret);
  if (mpq_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos, 1, NULL);
  mpz_mul(ret, x1, mpq_denref(x2));
  round_div(ret, ret, mpq_numref(x2));
  return ret;
}

CATALA_DEC o_div_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1,
                          CATALA_DURATION x2)
{
  long int days1, days2 = 0;
  CATALA_NEW_MPQ(ret);
  if (dc_period_to_days(&days1, x1) != dc_ok ||
      dc_period_to_days(&days2, x2) != dc_ok)
    catala_error(catala_uncomparable_durations, pos, 1, NULL);
  if (days2 == 0)
    catala_error(catala_division_by_zero, pos, 1, NULL);
  mpz_set_si(mpq_numref(ret), days1);
  mpz_set_si(mpq_denref(ret), days2);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_BOOL o_eq(const catala_type ty, const catala_code_position* pos,
                 const void* x1, const void* x2) {
  return CATALA_NEW_BOOL(catala_equal(ty, pos, x1, x2));
}

CATALA_BOOL o_lt (const catala_type ty, const catala_code_position* pos,
                  const void* x1, const void* x2) {
  return CATALA_NEW_BOOL(catala_compare(ty, pos, x1, x2) < 0);
}

CATALA_BOOL o_lte (const catala_type ty, const catala_code_position* pos,
                  const void* x1, const void* x2) {
  return CATALA_NEW_BOOL(catala_compare(ty, pos, x1, x2) <= 0);
}

CATALA_BOOL o_gt (const catala_type ty, const catala_code_position* pos,
                  const void* x1, const void* x2) {
  return CATALA_NEW_BOOL(catala_compare(ty, pos, x1, x2) > 0);
}

CATALA_BOOL o_gte (const catala_type ty, const catala_code_position* pos,
                  const void* x1, const void* x2) {
  return CATALA_NEW_BOOL(catala_compare(ty, pos, x1, x2) >= 0);
}

const CATALA_ARRAY(X) o_filter (catala_closure* cls, const CATALA_ARRAY(X) x)
{
  int i;
  CATALA_BOOL (*f)(const CLOSURE_ENV, const void*) =
    (CATALA_BOOL(*)(const CLOSURE_ENV, const void*))cls->funcp;
  catala_array* ret = catala_malloc(sizeof(catala_array));
  ret->size = 0;
  ret->elements = catala_malloc(x->size * sizeof(void*));
  for (i=0; i < x->size; i++) {
    CATALA_BOOL cond = f (cls->env, x->elements[i]);
    if (cond == CATALA_TRUE)
      ret->elements[ret->size++] = x->elements[i];
  }
  return ret;
}

const CATALA_ARRAY(Y) o_map (catala_closure* cls, const CATALA_ARRAY(X) x)
{
  int i;
  void* (*f)(const CLOSURE_ENV, const void*) =
    (void* (*)(const CLOSURE_ENV, const void*))cls->funcp;
  catala_array* ret = catala_malloc(sizeof(catala_array));
  ret->size = x->size;
  ret->elements = catala_malloc(x->size * sizeof(void*));
  for (i=0; i < x->size; i++)
    ret->elements[i] = f (cls->env, x->elements[i]);
  return ret;
}

const void* o_fold (catala_closure* cls,
                    const void* init, const CATALA_ARRAY(X) x)
{
  int i;
  const void* acc = init;
  void* (*f)(const CLOSURE_ENV, const void*, const void*) =
    (void* (*)(const CLOSURE_ENV, const void*, const void*))cls->funcp;
  for (i=0; i < x->size; i++)
    acc = f (cls->env, acc, x->elements[i]);
  return acc;
}

const void* o_reduce (catala_closure* cls,
                      catala_closure* dft, const CATALA_ARRAY(X) x)
{
  int i;
  const void* acc;
  void* (*f)(const CLOSURE_ENV, const void*, const void*) =
    (void* (*)(const CLOSURE_ENV, const void*, const void*))cls->funcp;
  if (x->size == 0) {
    void* (*dft_f)(const CLOSURE_ENV, const void*) =
      (void* (*)(const CLOSURE_ENV, const void*))dft->funcp;
    return dft_f(dft->env, &catala_unitval);
  }
  acc = x->elements[0];
  for (i=1; i < x->size; i++)
    acc = f (cls->env, acc, x->elements[i]);
  return acc;
}

const CATALA_ARRAY(Z) o_map2 (const catala_code_position* pos,
                        catala_closure* cls,
                        const CATALA_ARRAY(X) x,
                        const CATALA_ARRAY(Y) y)
{
  int i;
  void* (*f)(const CLOSURE_ENV, const void*, const void*) =
    (void* (*)(const CLOSURE_ENV, const void*, const void*))cls->funcp;
  catala_array* ret = catala_malloc(sizeof(catala_array));
  if (x->size != y->size)
    catala_error(catala_not_same_length, pos, 1, NULL);
  ret->size = x->size;
  ret->elements = catala_malloc(ret->size * sizeof(void*));
  for (i=0; i < x->size; i++)
    ret->elements[i] = f (cls->env, x->elements[i], y->elements[i]);
  return ret;
}

const CATALA_ARRAY(Z) o_concat (const CATALA_ARRAY(X) x,
                                const CATALA_ARRAY(Y) y)
{
  int i;
  catala_array* ret = catala_malloc(sizeof(catala_array));
  ret->size = x->size + y->size;
  ret->elements = catala_malloc(ret->size * sizeof(void*));
  for (i=0; i < x->size; i++)
    ret->elements[i] = x->elements[i];
  for (; i < ret->size; i++)
    ret->elements[i] = y->elements[i - x->size];
  return ret;
}

const catala_option catala_none = {catala_option_none, NULL};

const CATALA_OPTION(X) catala_some (const void* x) {
  catala_option* ret;
  ret = catala_malloc(sizeof(catala_option));
  ret->code = catala_option_some;
  ret->payload = x;
  return ret;
}

CATALA_BOOL catala_isnone (const CATALA_OPTION() opt)
{
  return CATALA_NEW_BOOL(opt->code == catala_option_none);
}

const CATALA_EXN(_) handle_exceptions
  (const CATALA_ARRAY(const CATALA_EXN) e)
{
  int i, j;
  unsigned int size = e->size;
  const CATALA_EXN(_) * excs = (const CATALA_OPTION() *)e->elements;
  for (i = 0; i < size && excs[i]->code == catala_option_none; i++) {}
  if (i >= size) return CATALA_NONE;
  for(j = i + 1; j < size && excs[j]->code == catala_option_none; j++) {}
  if (j < size) {
    int k, count;
    catala_code_position * pos =
      catala_malloc (size * sizeof(catala_code_position));
    pos[0] = *(catala_code_position *)((CATALA_TUPLE(_))(excs[i]->payload))[1].content;
    for (k = j, count = 1; k < size; k++)
      if (excs[k]->code == catala_option_some)
        pos[count++] = *(catala_code_position *)((CATALA_TUPLE(_))(excs[k]->payload))[1].content;
    catala_error(catala_conflict, pos, count, NULL);
  }
  return excs[i];
}


void* (*error_handler)(const struct catala_error *) = NULL;

void register_error_handler(void* (*f)(const struct catala_error *)){
  error_handler = f;
}

void catala_init(void)
{
  mpz_init_set_ui(zconst_100, 100);
  mp_set_memory_functions(&catala_malloc,&catala_realloc,&catala_free);
  catala_persistent_malloc_mode_on = 0;
}

void* catala_do(void* (*f)(void))
{
  void* retval;
  catala_init();
  if (setjmp(catala_error_jump_buffer)) {
    char *error_kind;
    int i;
    const catala_code_position * pos = catala_error_raised.position;
    if (error_handler != NULL) {
      return error_handler(&catala_error_raised);
    }
    switch (catala_error_raised.code) {
    case catala_assertion_failed:
      error_kind = "Assertion failure";
      break;
    case catala_no_value:
      error_kind = "No value provided";
      break;
    case catala_conflict:
      error_kind = "Conflict between exceptions";
      break;
    case catala_division_by_zero:
      error_kind = "Division by zero";
      break;
    case catala_list_empty:
      error_kind = "Empty list";
      break;
    case catala_not_same_length:
      error_kind = "List lengths not matching";
      break;
    case catala_invalid_date:
      error_kind = "the provided numbers do not correspond to a valid date";
      break;
    case catala_uncomparable_durations:
      error_kind = "Comparison between incompatible durations";
      break;
    case catala_ambiguous_date_rounding:
      error_kind = "Ambiguous date computation, and rounding mode was not specified";
      break;
    case catala_indivisible_durations:
      error_kind = "Division of incompatible durations";
      break;
    case catala_impossible:
      error_kind = "\"impossible\" computation reached";
      break;
    case catala_malloc_error:
      error_kind = "Out of memory";
      break;
    }
    /* Prints in the same format as the OCaml runtime. The messages differ though. */
    fprintf(stderr, "\033[1;31m[ERROR]\033[m ");
    for (i = 0; i < catala_error_raised.nb_positions; i++) {
      if (pos[i].filename)
        fprintf(stderr, "%s %s:%d.%d-%d.%d",
                i == 0 ? "At" : ",",
                pos[i].filename,
                pos[i].start_line,
                pos[i].start_column,
                pos[i].end_line,
                pos[i].end_column);
    }
    fprintf(stderr, "%s%s", i > 0 ? ": " : "", error_kind);
    if (catala_error_raised.note)
      fprintf(stderr, ". %s", catala_error_raised.note);
    fprintf(stderr, "\n");
    return NULL;
  }
  has_jump_buffer = 1;
  retval = f();
  has_jump_buffer = 0;
  return retval;
}
