#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <gmp.h>
#include <string.h>

/* --- Error handling --- */

typedef enum catala_error_code
{
  catala_assertion_failed,
  catala_no_value,
  catala_conflict,
  catala_division_by_zero,
  catala_not_same_length,
  catala_uncomparable_durations,
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
  catala_code_position position;
  catala_error_code code;
};

const catala_code_position catala_empty_position =
  { NULL, 0, 0, 0, 0 };

struct catala_error catala_error_raised =
  { catala_empty_position, 0 };

jmp_buf catala_error_jump_buffer;

void catala_error(catala_error_code code,
                  const catala_code_position * pos)
{
  catala_error_raised.code = code;
  catala_error_raised.position = *pos;
  longjmp(catala_error_jump_buffer, 1);
}

/* --- Memory allocations --- */

#define BLOCKSIZE 32 /* TODO:
                        For stressing the allocator for testing purposes only,
                        should be reverted to something like 4096 or 16384 */

struct catala_heap
{
  void* mem;    /* Start of the allocated block */
  void* curptr; /* Pointer to the first unattributed yet byte in mem */
  void* end;    /* End boundary of the allocated block (excluded) */
  struct catala_heap* next;
};

struct catala_heap catala_heap = {NULL, NULL, NULL, NULL};

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
  if (nextptr < catala_heap.end) {
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

void catala_free_all()
{
  while (catala_heap.mem != NULL) {
    free(catala_heap.mem);
    catala_heap = *catala_heap.next;
  }
}

void* catala_realloc(void* oldptr, size_t oldsize, size_t newsize)
{
  if (newsize <= oldsize) {
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

/* --- Base types --- */

#define CATALA_BOOL const int*
#define CATALA_UNIT const void*
#define CATALA_INT mpz_srcptr
#define CATALA_DEC mpq_srcptr
#define CATALA_MONEY mpz_srcptr
#define CATALA_DATE const catala_date*
#define CATALA_DURATION const catala_duration*
#define CATALA_ARRAY(_) catala_array*
#define CATALA_TUPLE void**

#define CLOSURE_ENV void**

typedef struct catala_closure {
  void (*funcp)(void);
  const CLOSURE_ENV env;
} catala_closure;

const int catala_true = 1;
#define CATALA_TRUE &catala_true

const int catala_false = 0;
#define CATALA_FALSE &catala_false

const int catala_unitval = 0;
#define CATALA_UNITVAL &catala_unitval

typedef struct catala_date {
  unsigned long int year;
  unsigned long int month;
  unsigned long int day;
} catala_date;

typedef struct catala_duration {
  long int years;
  long int months;
  long int days;
} catala_duration;

typedef struct catala_array {
  size_t size;
  void const ** elements;
} catala_array;

/* --- Constructors --- */

static mpz_t zconst_100;

#define CATALA_NEW_BOOL(X) \
  X ? CATALA_TRUE : CATALA_FALSE

#define CATALA_NEW_MPZ(X) \
  mpz_ptr X = catala_malloc(sizeof(__mpz_struct)); \
  mpz_init(X)

#define CATALA_NEW_MPQ(X) \
  mpq_ptr X = catala_malloc(sizeof(__mpq_struct)); \
  mpq_init(X)

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

CATALA_INT catala_new_money(const signed long int val)
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

CATALA_DATE catala_new_date(const unsigned int year,
                     const unsigned int month,
                     const unsigned int day)
{
  catala_date* ret = catala_malloc(sizeof(catala_date));
  ret->year = year;
  ret->month = month;
  ret->day = day;
  return ret;
}

CATALA_DURATION catala_new_duration(const int years,
                             const int months,
                             const int days)
{
  catala_duration* ret = catala_malloc(sizeof(catala_duration));
  ret->years = years;
  ret->months = months;
  ret->days = days;
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
  return catala_new_int(date->day);
}

CATALA_INT o_getMonth(CATALA_DATE date)
{
  return catala_new_int(date->month);
}

CATALA_INT o_getYear(CATALA_DATE date)
{
  return catala_new_int(date->year);
}

CATALA_DATE o_firstDayOfMonth(CATALA_DATE date)
{
  return catala_new_date(date->year, date->month, 1);
}

CATALA_DATE o_lastDayOfMonth(CATALA_DATE date)
{
  int ndays;
  switch (date->month) {
  case 2:
    ndays =
      (date->year % 400 == 0 || (date->year % 4 == 0 && date->year % 100 != 0))
      ? 29 : 28;
    break;
  case 4:
  case 6:
  case 9:
  case 11:
    ndays = 30; break;
  default:
    ndays = 31;
  }
  return catala_new_date(date->year, date->month, ndays);
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
  return catala_new_duration(-dur->years, -dur->months, -dur->days);
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

CATALA_MONEY o_tomoney_rat (CATALA_DEC x)
{
  CATALA_NEW_MPZ(ret);
  mpz_mul_ui(ret, mpq_numref(x), 100);
  round_div(ret, ret, mpq_denref(x));
  return ret;
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

typedef enum catala_date_rounding
{
  catala_date_round_up,
  catala_date_round_down,
  catala_date_round_abort
} catala_date_rounding;

CATALA_DATE o_add_dat_dur (catala_date_rounding mode,
                           const catala_code_position* pos,
                           CATALA_DATE x1,
                           CATALA_DURATION x2)
{
  /* TODO */
  return catala_new_date(x1->year + x2->years,
                         x1->month + x2->months,
                         x1->day + x2->days);
}

CATALA_DURATION o_add_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2)
{
  return catala_new_duration(x1->years + x2->years,
                      x1->months + x2->months,
                      x1->days + x2->days);
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
  /* FIXME: all date/duration operations need to be implemented following
     dates_calc */
  return catala_new_duration(x1->year - x2->year,
                      x1->month - x2->month,
                      x1->day - x2->day);
}

CATALA_DATE o_sub_dat_dur (CATALA_DATE x1, CATALA_DURATION x2)
{
  return catala_new_date(x1->year - x2->years,
                  x1->month - x2->months,
                  x1->day - x2->days);
}

CATALA_DURATION o_sub_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2)
{
  return catala_new_duration(x1->years - x2->years,
                      x1->months - x2->months,
                      x1->days - x2->days);
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
  return catala_new_duration(x1->years * mult,
                      x1->months * mult,
                      x1->days * mult);
}

CATALA_DEC o_div_int_int (const catala_code_position* pos,
                          CATALA_INT x1,
                          CATALA_INT x2)
{
  CATALA_NEW_MPQ(ret);
  if (mpz_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos);
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
    catala_error(catala_division_by_zero, pos);
  mpq_div(ret, x1, x2);
  return ret;
}

CATALA_DEC o_div_mon_mon (const catala_code_position* pos,
                          CATALA_MONEY x1,
                          CATALA_MONEY x2)
{
  CATALA_NEW_MPQ(ret);
  if (mpz_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos);
  mpz_set(mpq_numref(ret), x2);
  mpz_set(mpq_denref(ret), x1);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_MONEY o_div_mon_rat (const catala_code_position* pos,
                            CATALA_MONEY x1,
                            CATALA_DEC x2)
{
  CATALA_NEW_MPZ(ret);
  if (mpq_sgn(x2) == 0)
    catala_error(catala_division_by_zero, pos);
  mpz_mul(ret, x1, mpq_denref(x2));
  round_div(ret, ret, mpq_numref(x2));
  return ret;
}

CATALA_DEC o_div_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1,
                          CATALA_DURATION x2)
{
  const signed long int days1 = x1->days;
  const signed long int days2 = x2->days;
  CATALA_NEW_MPQ(ret);
  if (x1->years || x2->years || x1->months || x2->months)
    catala_error(catala_indivisible_durations, pos);
  if (days2 == 0)
    catala_error(catala_division_by_zero, pos);
  mpz_set_si(mpq_numref(ret), days1);
  mpz_set_si(mpq_denref(ret), days2);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_BOOL o_eq_boo_boo (CATALA_BOOL x1, CATALA_BOOL x2) {
  return CATALA_NEW_BOOL(*x1 == *x2);
}

CATALA_BOOL o_eq_int_int (CATALA_INT x1, CATALA_INT x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) == 0);
}

CATALA_BOOL o_eq_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return CATALA_NEW_BOOL(mpq_equal(x1, x2));
}

CATALA_BOOL o_eq_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) == 0);
}

CATALA_BOOL o_eq_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return CATALA_NEW_BOOL((x1->year == x2->year && x1->month == x2->month && x1->day == x2->day));
}

CATALA_BOOL o_eq_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_error(catala_uncomparable_durations, pos);
  return CATALA_NEW_BOOL(x1->days == x2->days);
}

CATALA_BOOL o_lt_int_int (CATALA_INT x1, CATALA_INT x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) < 0);
}

CATALA_BOOL o_lt_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return CATALA_NEW_BOOL(mpq_cmp(x1, x2) < 0);
}

CATALA_BOOL o_lt_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) < 0);
}

CATALA_BOOL o_lt_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    CATALA_NEW_BOOL
    ((x1->year < x2->year) ||
     (x1->year == x2->year &&
      ((x1->month < x2->month) ||
       (x1->month == x2->month && x1->day < x2->day))));
}

CATALA_BOOL o_lt_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_error(catala_uncomparable_durations, pos);
  return CATALA_NEW_BOOL(x1->days < x2->days);
}

CATALA_BOOL o_lte_int_int (CATALA_INT x1, CATALA_INT x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) <= 0);
}

CATALA_BOOL o_lte_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return CATALA_NEW_BOOL(mpq_cmp(x1, x2) <= 0);
}

CATALA_BOOL o_lte_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) <= 0);
}

CATALA_BOOL o_lte_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    CATALA_NEW_BOOL
    ((x1->year < x2->year) ||
     (x1->year == x2->year &&
      ((x1->month < x2->month) ||
      (x1->month == x2->month && x1->day <= x2->day))));
}

CATALA_BOOL o_lte_dur_dur (const catala_code_position* pos,
                           CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_error(catala_uncomparable_durations, pos);
  return CATALA_NEW_BOOL(x1->days <= x2->days);
}

CATALA_BOOL o_gt_int_int (CATALA_INT x1, CATALA_INT x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) > 0);
}

CATALA_BOOL o_gt_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return CATALA_NEW_BOOL(mpq_cmp(x1, x2) > 0);
}

CATALA_BOOL o_gt_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) > 0);
}

CATALA_BOOL o_gt_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    CATALA_NEW_BOOL
    ((x1->year > x2->year) ||
     (x1->year == x2->year &&
      ((x1->month > x2->month) ||
       (x1->month == x2->month && x1->day > x2->day))));
}

CATALA_BOOL o_gt_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_error(catala_uncomparable_durations, pos);
  return CATALA_NEW_BOOL(x1->days > x2->days);
}

CATALA_BOOL o_gte_int_int (CATALA_INT x1, CATALA_INT x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) >= 0);
}

CATALA_BOOL o_gte_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return CATALA_NEW_BOOL(mpq_cmp(x1, x2) >= 0);
}

CATALA_BOOL o_gte_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return CATALA_NEW_BOOL(mpz_cmp(x1, x2) >= 0);
}

CATALA_BOOL o_gte_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    CATALA_NEW_BOOL
    ((x1->year > x2->year) ||
     (x1->year == x2->year &&
      ((x1->month > x2->month) ||
       (x1->month == x2->month && x1->day >= x2->day))));
}

CATALA_BOOL o_gte_dur_dur (const catala_code_position* pos,
                           CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_error(catala_uncomparable_durations, pos);
  return CATALA_NEW_BOOL(x1->days >= x2->days);
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
    CATALA_BOOL cond;
    cond = f (cls->env, x->elements[i]);
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
                      const void* dft, const CATALA_ARRAY(X) x)
{
  int i;
  const void* acc;
  void* (*f)(const CLOSURE_ENV, const void*, const void*) =
    (void* (*)(const CLOSURE_ENV, const void*, const void*))cls->funcp;
  if (x->size == 0) return dft;
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
    catala_error(catala_not_same_length, pos);
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

enum catala_option_code {
  catala_option_none,
  catala_option_some
};

typedef struct catala_option {
  enum catala_option_code code;
  const void* payload;
} catala_option;

#define CATALA_OPTION(_) catala_option*

const catala_option catala_none = {catala_option_none, NULL};

#define CATALA_NONE &catala_none

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

const CATALA_OPTION() handle_exceptions
  (const CATALA_ARRAY(const catala_code_position*) pos,
   const CATALA_ARRAY(const CATALA_OPTION()) e)
{
  int i, j;
  unsigned int size = e->size;
  const CATALA_OPTION() * excs = (const CATALA_OPTION() *)e->elements;
  for (i = 0; i < size && excs[i]->code == catala_option_none; i++) {}
  if (i >= size) return CATALA_NONE;
  for(j = i + 1; j < size && excs[j]->code == catala_option_none; j++) {}
  if (j < size) catala_error(catala_conflict, pos->elements[j]);
  return excs[i];
}

void catala_init()
{
  mp_set_memory_functions(&catala_malloc,&catala_realloc,&catala_free);
  mpz_init_set_ui(zconst_100, 100);
  if (setjmp(catala_error_jump_buffer)) {
    char *error_kind;
    const catala_code_position pos = catala_error_raised.position;
    switch (catala_error_raised.code) {
    case catala_assertion_failed:
      error_kind = "Asssertion failure";
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
    case catala_not_same_length:
      error_kind = "List lengths not matching";
      break;
    case catala_uncomparable_durations:
      error_kind = "Comparison between incompatible durations";
      break;
    case catala_indivisible_durations:
      error_kind = "Division of incompatible durations";
      break;
    case catala_malloc_error:
      error_kind = "Out of memory";
      break;
    }
    if (pos.filename == NULL)
      printf("\033[1;31m[ERROR]\033[m %s\n", error_kind);
    else
      printf("\033[1;31m[ERROR]\033[m %s in file %s:%d.%d-%d.%d\n",
             error_kind,
             pos.filename,
             pos.start_line,
             pos.start_column,
             pos.end_line,
             pos.end_column);
    catala_free_all();
    exit(10);
  }
  return;
}

/* TODO: this is a basic unit test for the runtime. It should be moved to a
 * separate file and integrated in the build system. (And extended)
 *
 * void test()
 * {
 *   CATALA_MONEY dollars = catala_new_money_str ("10000");
 *   CATALA_DEC rate = catala_new_dec_str ("1/3");
 *   CATALA_DEC rate2 = catala_new_dec_str ("30/100");
 *   CATALA_DEC ratex = o_add_rat_rat(rate, rate2);
 *   CATALA_MONEY result = o_mult_mon_rat(dollars,ratex);
 *   mpz_out_str(NULL,10,result);
 *   printf("\n");
 *   fflush(stdout);
 * }
 * 
 * int main()
 * {
 *   catala_init();
 *   test();
 *   catala_free_all();
 *   return 0;
 * } */
