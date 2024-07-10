#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <gmp.h>
#include <string.h>

/* --- Error handling --- */

typedef enum catala_fatal_error_code
{
  catala_assertion_failed,
  catala_no_value,
  catala_conflict,
  catala_division_by_zero,
  catala_not_same_length,
  catala_uncomparable_durations,
  catala_indivisible_durations,
  catala_malloc_error
} catala_fatal_error_code;

typedef struct catala_code_position
{
    char *filename;
    unsigned int start_line;
    unsigned int start_column;
    unsigned int end_line;
    unsigned int end_column;
} catala_code_position;

typedef struct catala_fatal_error
{
  const catala_code_position * position;
  catala_fatal_error_code code;
} catala_fatal_error;

catala_fatal_error catala_fatal_error_raised = {NULL, 0};

jmp_buf catala_fatal_error_jump_buffer;

void catala_raise_fatal_error(catala_fatal_error_code code,
                              const catala_code_position * pos)
{
  catala_fatal_error_raised.code = code;
  catala_fatal_error_raised.position = pos;
  longjmp(catala_fatal_error_jump_buffer, 1);
}

/* --- Memory allocations --- */

#define BLOCKSIZE 4096

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
    catala_fatal_error_raised.code = catala_malloc_error;   \
    longjmp(catala_fatal_error_jump_buffer, 1);             \
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
    struct catala_heap* next = (struct catala_heap*)malloc(sizeof(catala_heap));
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
    memset(oldptr + newsize, oldsize - newsize, 0);
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

#define CATALA_BOOL int
#define CATALA_TRUE 1
#define CATALA_FALSE 0
#define CATALA_UNIT int
#define CATALA_INT mpz_srcptr
#define CATALA_DEC mpq_srcptr
#define CATALA_MONEY mpz_srcptr
#define CATALA_DATE const catala_date*
#define CATALA_DURATION const catala_duration*
#define CATALA_ARRAY const catala_array

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
  unsigned int size;
  unsigned int eltsize;
  void* elements;
} catala_array;

/* --- Constructors --- */

static mpz_t zconst_100;

#define CATALA_NEW_MPZ(X) \
  mpz_ptr X = (mpz_ptr)catala_malloc(sizeof(__mpz_struct)); \
  mpz_init(X)

#define CATALA_NEW_MPQ(X) \
  mpq_ptr X = (mpq_ptr)catala_malloc(sizeof(__mpq_struct)); \
  mpq_init(X)

CATALA_INT new_int(const signed long int val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_si(ret, val);
  return ret;
}

/* Arg is a null-terminated string */
CATALA_INT new_int_str(const char* val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_str(ret, val, 10);
  return ret;
}

CATALA_DEC new_dec (const signed long int units,
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

/* Arg is a null-terminated string that must be in fraction form (eg 1234/100, not 12.34) */
CATALA_DEC new_dec_str(const char* val)
{
  CATALA_NEW_MPQ(ret);
  mpq_set_str(ret, val, 10);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_INT new_money(const signed long int val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_si(ret, val);
  return ret;
}

/* Arg is a null-terminated string */
CATALA_MONEY new_money_str(const char* val)
{
  CATALA_NEW_MPZ(ret);
  mpz_set_str(ret, val, 10);
  return ret;
}

CATALA_DATE new_date(const unsigned int year,
                     const unsigned int month,
                     const unsigned int day)
{
  catala_date* ret = (catala_date *)catala_malloc(sizeof(catala_date));
  ret->year = year;
  ret->month = month;
  ret->day = day;
  return ret;
}

CATALA_DURATION new_duration(const int years,
                             const int months,
                             const int days)
{
  catala_duration* ret =
    (catala_duration *)catala_malloc(sizeof(catala_duration));
  ret->years = years;
  ret->months = months;
  ret->days = days;
  return ret;
}

/* --- Operators --- */

CATALA_BOOL o_not(int b)
{
  return (! b);
}

CATALA_INT o_length(CATALA_ARRAY arr)
{
  return new_int(arr.size);
}

CATALA_INT o_getDay(CATALA_DATE date)
{
  return new_int(date->day);
}

CATALA_INT o_getMonth(CATALA_DATE date)
{
  return new_int(date->month);
}

CATALA_INT o_getYear(CATALA_DATE date)
{
  return new_int(date->year);
}

CATALA_DATE o_firstDayOfMonth(CATALA_DATE date)
{
  return new_date(date->year, date->month, 1);
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
  return new_date(date->year, date->month, ndays);
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
  return new_duration(-dur->years, -dur->months, -dur->days);
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
  mpz_init_set(den1, den);
  mpz_set(ret, num);
  /* if x = n/d, this is (2*n + d) / (2*d) */
  mpz_mul_ui(ret, ret, 2);
  mpz_add(ret, ret, den1);
  mpz_mul_ui(den1, den1, 2);
  /* round towards 0 */
  mpz_tdiv_q(ret, ret, den1);
  mpz_clear(den1);
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
  return x1 && x2;
}

CATALA_BOOL o_or (CATALA_BOOL x1, CATALA_BOOL x2)
{
  return x1 || x2;
}

CATALA_BOOL o_xor (CATALA_BOOL x1, CATALA_BOOL x2)
{
  return x1 != x2;
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

CATALA_DATE add_dat_dur (catala_date_rounding mode,
                         CATALA_DATE x1,
                         CATALA_DURATION x2)
{
  /* TODO */
  return new_date(x1->year + x2->years,
                  x1->month + x2->months,
                  x1->day + x2->days);
}

CATALA_DURATION o_add_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2)
{
  return new_duration(x1->years + x2->years,
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
  return new_duration(x1->year - x2->year,
                      x1->month - x2->month,
                      x1->day - x2->day);
}

CATALA_DATE o_sub_dat_dur (CATALA_DATE x1, CATALA_DURATION x2)
{
  return new_date(x1->year - x2->years,
                  x1->month - x2->months,
                  x1->day - x2->days);
}

CATALA_DURATION o_sub_dur_dur (CATALA_DURATION x1, CATALA_DURATION x2)
{
  return new_duration(x1->years - x2->years,
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
  return new_duration(x1->years * mult,
                      x1->months * mult,
                      x1->days * mult);
}

CATALA_DEC o_div_int_int (const catala_code_position* pos,
                          CATALA_INT x1,
                          CATALA_INT x2)
{
  CATALA_NEW_MPQ(ret);
  if (mpz_sgn(x2) == 0)
    catala_raise_fatal_error(catala_division_by_zero, pos);
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
    catala_raise_fatal_error(catala_division_by_zero, pos);
  mpq_div(ret, x1, x2);
  return ret;
}

CATALA_DEC o_div_mon_mon (const catala_code_position* pos,
                          CATALA_MONEY x1,
                          CATALA_MONEY x2)
{
  CATALA_NEW_MPQ(ret);
  if (mpz_sgn(x2) == 0)
    catala_raise_fatal_error(catala_division_by_zero, pos);
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
    catala_raise_fatal_error(catala_division_by_zero, pos);
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
    catala_raise_fatal_error(catala_indivisible_durations, pos);
  if (days2 == 0)
    catala_raise_fatal_error(catala_division_by_zero, pos);
  mpz_set_si(mpq_numref(ret), days1);
  mpz_set_si(mpq_denref(ret), days2);
  mpq_canonicalize(ret);
  return ret;
}

CATALA_BOOL o_eq_bool_bool (CATALA_BOOL x1, CATALA_BOOL x2) {
  return x1 == x2;
}

CATALA_BOOL o_eq_int_int (CATALA_INT x1, CATALA_INT x2) {
  return mpz_cmp(x1, x2) == 0;
}

CATALA_BOOL o_eq_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return mpq_equal(x1, x2);
}

CATALA_BOOL o_eq_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return mpz_cmp(x1, x2) == 0;
}

CATALA_BOOL o_eq_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return (x1->year == x2->year && x1->month == x2->month && x1->day == x2->day);
}

CATALA_BOOL o_eq_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_raise_fatal_error(catala_uncomparable_durations, pos);
  return x1->days == x2->days;
}

CATALA_BOOL o_lt_int_int (CATALA_INT x1, CATALA_INT x2) {
  return mpz_cmp(x1, x2) < 0;
}

CATALA_BOOL o_lt_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return mpq_cmp(x1, x2) < 0;
}

CATALA_BOOL o_lt_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return mpz_cmp(x1, x2) < 0;
}

CATALA_BOOL o_lt_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    (x1->year < x2->year) ||
    (x1->year == x2->year &&
     ((x1->month < x2->month) ||
      (x1->month == x2->month && x1->day < x2->day)));
}

CATALA_BOOL o_lt_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_raise_fatal_error(catala_uncomparable_durations, pos);
  return x1->days < x2->days;
}

CATALA_BOOL o_lte_int_int (CATALA_INT x1, CATALA_INT x2) {
  return mpz_cmp(x1, x2) <= 0;
}

CATALA_BOOL o_lte_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return mpq_cmp(x1, x2) <= 0;
}

CATALA_BOOL o_lte_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return mpz_cmp(x1, x2) <= 0;
}

CATALA_BOOL o_lte_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    (x1->year < x2->year) ||
    (x1->year == x2->year &&
     ((x1->month < x2->month) ||
      (x1->month == x2->month && x1->day <= x2->day)));
}

CATALA_BOOL o_lte_dur_dur (const catala_code_position* pos,
                           CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_raise_fatal_error(catala_uncomparable_durations, pos);
  return x1->days <= x2->days;
}

CATALA_BOOL o_gt_int_int (CATALA_INT x1, CATALA_INT x2) {
  return mpz_cmp(x1, x2) > 0;
}

CATALA_BOOL o_gt_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return mpq_cmp(x1, x2) > 0;
}

CATALA_BOOL o_gt_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return mpz_cmp(x1, x2) > 0;
}

CATALA_BOOL o_gt_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    (x1->year > x2->year) ||
    (x1->year == x2->year &&
     ((x1->month > x2->month) ||
      (x1->month == x2->month && x1->day > x2->day)));
}

CATALA_BOOL o_gt_dur_dur (const catala_code_position* pos,
                          CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_raise_fatal_error(catala_uncomparable_durations, pos);
  return x1->days > x2->days;
}

CATALA_BOOL o_gte_int_int (CATALA_INT x1, CATALA_INT x2) {
  return mpz_cmp(x1, x2) >= 0;
}

CATALA_BOOL o_gte_rat_rat (CATALA_DEC x1, CATALA_DEC x2) {
  return mpq_cmp(x1, x2) >= 0;
}

CATALA_BOOL o_gte_mon_mon (CATALA_MONEY x1, CATALA_MONEY x2) {
  return mpz_cmp(x1, x2) >= 0;
}

CATALA_BOOL o_gte_dat_dat (CATALA_DATE x1, CATALA_DATE x2) {
  return
    (x1->year > x2->year) ||
    (x1->year == x2->year &&
     ((x1->month > x2->month) ||
      (x1->month == x2->month && x1->day >= x2->day)));
}

CATALA_BOOL o_gte_dur_dur (const catala_code_position* pos,
                           CATALA_DURATION x1, CATALA_DURATION x2) {
  if (x1->years || x2->years || x1->months || x2->months)
    catala_raise_fatal_error(catala_uncomparable_durations, pos);
  return x1->days >= x2->days;
}

/*
TODO

o_eq
o_map
o_map2
o_concat
o_filter
o_reduce
*/



void catala_init()
{
  mp_set_memory_functions(&catala_malloc,&catala_realloc,&catala_free);
  mpz_init_set_ui(zconst_100, 100);
  if (setjmp(catala_fatal_error_jump_buffer)) {
    char *error_kind;
    const catala_code_position* pos = catala_fatal_error_raised.position;
    switch (catala_fatal_error_raised.code) {
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
    if (pos == NULL)
      printf("\033[1;31m[ERROR]\033[m %s\n", error_kind);
    else
      printf("\033[1;31m[ERROR]\033[m %s in file %s:%d.%d-%d.%d\n",
             error_kind,
             pos->filename,
             pos->start_line,
             pos->start_column,
             pos->end_line,
             pos->end_column);
    catala_free_all();
    exit(10);
  }
  return;
}

void test()
{
  CATALA_MONEY dollars = new_money_str ("10000");
  CATALA_DEC rate = new_dec_str ("1/3");
  CATALA_DEC rate2 = new_dec_str ("30/100");
  CATALA_DEC ratex = o_add_rat_rat(rate, rate2);
  CATALA_MONEY result = o_mult_mon_rat(dollars,ratex);
  mpz_out_str(NULL,10,result);
  printf("\n");
  fflush(stdout);
}

int main()
{
  catala_init();
  test();
  catala_free_all();
  return 0;
}
