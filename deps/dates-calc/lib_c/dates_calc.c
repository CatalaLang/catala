/* This file is part of the Dates_calc library. Copyright (C) 2024 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>, Raphaël Monat
   <raphael.monat@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. */

#include <assert.h>
#include <stdio.h>

#define BOOL int
#define FALSE 0
#define TRUE 1

/* Layout of this C version:
   - types and functions in this module are prefixed with [dc_]
   - dates and periods are manipulated as pointers to the defined structs
   - functions return through a first [ret] pointer argument, the other
     arguments are [const].
   - functions that can fail return the [dc_success] type. What is stored into
     [ret] is unspecified when that is [dc_error].
   - it is expected that [ret] and other arguments may overlap, so, in the code
     below, a field in [ret] should never be written before the same field in
     any other argument of the same type is read.
*/

typedef enum dc_success {
  dc_error, dc_ok
} dc_success;

typedef enum dc_date_rounding {
  dc_date_round_up,
  dc_date_round_down,
  dc_date_round_abort
} dc_date_rounding;


typedef struct dc_date {
  long int year;
  unsigned long int month;
  unsigned long int day;
} dc_date;

typedef struct dc_period {
  long int years;
  long int months;
  long int days;
} dc_period;

void dc_make_period(dc_period *ret, const long int y, const long int m, const long int d) {
  ret->years = y;
  ret->months = m;
  ret->days = d;
}

void dc_print_period (const dc_period *p) {
  printf("[%ld years, %ld months, %ld days]", p->years, p->months, p->days);
}

dc_success dc_period_of_string (dc_period *ret, const char* s) {
  if
    (sscanf(s, "[%ld years, %ld months, %ld days]",
            &ret->years, &ret->months, &ret->days)
     == 3)
    return dc_ok;
  else
    return dc_error;
}

void dc_add_periods (dc_period *ret, const dc_period *p1, const dc_period *p2) {
  ret->years = p1->years + p2->years;
  ret->months = p1->months + p2->months;
  ret->days = p1->days + p2->days;
}

void dc_sub_periods (dc_period *ret, const dc_period *p1, const dc_period *p2) {
  ret->years = p1->years - p2->years;
  ret->months = p1->months - p2->months;
  ret->days = p1->days - p2->days;
}

void dc_mul_periods (dc_period *ret, const dc_period *p, const long int m) {
  ret->years = p->years * m;
  ret->months = p->months * m;
  ret->days = p->days * m;
}

dc_success dc_period_to_days (long int *ret, const dc_period *p) {
  if (p->years || p->months) return dc_error;
  else *ret = p->days;
  return dc_ok;
}

BOOL dc_is_leap_year (const long int y) {
  return (y % 400 == 0 || (y % 4 == 0 && y % 100 != 0));
}

unsigned long int dc_days_in_month (const dc_date *d) {
  switch (d->month) {
  case 2:
    return (dc_is_leap_year(d->year) ? 29 : 28);
  case 4: case 6: case 9: case 11:
    return 30;
  default:
    return 31;
  }
}

BOOL dc_is_valid_date (const dc_date *d) {
  return (1 <= d->day && d->day <= dc_days_in_month(d));
}

dc_success dc_make_date(dc_date *ret, const long int y, const unsigned long int m, const unsigned long int d) {
  ret->year = y;
  ret->month = m;
  ret->day = d;
  if (dc_is_valid_date(ret)) return dc_ok;
  else return dc_error;
}

void dc_copy_date(dc_date *ret, const dc_date *d) {
  if (ret != d) {
    ret->year = d->year;
    ret->month = d->month;
    ret->day = d->day;
  }
}

/* Precondition: [1 <= d->month <= 12]. The returned day is always [1] */
void dc_add_months(dc_date *ret, const dc_date *d, const long int months) {
  long int month = d->month - 1 + months;
  /* The month variable is shifted -1 to be in range [0, 11] for modulo
     calculations */
  /*  assert (1 <= d->month && d->month <= 12); */
  ret->day = 1;
  ret->month = (month >= 0 ? month % 12 : month % 12 + 12) + 1;
  ret->year = d->year + (month >= 0 ? month / 12 : month / 12 - 1);
}

/* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the last
   day of the current month. */
void dc_prev_valid_date (dc_date *ret, const dc_date *d) {
  assert (1 <= d->month && d->month <= 12);
  assert (1 <= d->day && d->day <= 31);
  if (dc_is_valid_date(d))
    dc_copy_date(ret, d);
  else {
    ret->year = d->year;
    ret->month = d->month;
    ret->day = dc_days_in_month(d);
  }
}

/* If the date is valid, does nothing. We expect the month number to be always
   valid when calling this. If the date is invalid due to the day number, then
   this function rounds down: if the day number is >= days_in_month, to the
   first day of the next month. */
void dc_next_valid_date (dc_date *ret, const dc_date *d) {
  assert (1 <= d->month && d->month <= 12);
  assert (1 <= d->day && d->day <= 31);
  if (dc_is_valid_date(d))
    dc_copy_date(ret, d);
  else {
    dc_add_months (ret, d, 1);
  }
}

dc_success dc_round_date (dc_date *ret, const dc_date_rounding rnd, const dc_date *d) {
  if (dc_is_valid_date(d)) {
    dc_copy_date(ret, d);
    return dc_ok;
  } else switch (rnd) {
    case dc_date_round_down:
      dc_prev_valid_date(ret, d);
      return dc_ok;
    case dc_date_round_up:
      dc_next_valid_date(ret, d);
      return dc_ok;
    default:
      return dc_error;
    }
}

void add_dates_days (dc_date *ret, const dc_date *d, const long int days) {
  unsigned long int days_in_d_month;
  unsigned long int day_num;
  /* Hello, dear reader! Buckle up because it will be a hard ride. The first
     thing to do here is to retrieve how many days there are in the current
     month of [d]. */
  days_in_d_month = dc_days_in_month(d);
  /* Now, we case analyze of the situation. To do that, we add the current days
     of the month with [days], and see what happens. Beware, [days] is algebraic
     and can be negative! */
  day_num = d->day + days;
  if (day_num < 1) {
    /* we substracted too many days and the current month can't handle it. So we
       warp to the previous month and let a recursive call handle the situation
       from there. */
    dc_date d1;
    /* We warp to the last day of the previous month. */
    dc_add_months(&d1, d, -1);
    d1.day = dc_days_in_month(&d1);
    /* What remains to be substracted (as [days] is negative) has to be
       diminished by the number of days of the date in the current month. */
    add_dates_days(ret, &d1, days + d->day);
  } else if (days_in_d_month < day_num) {
    /* Here there is an overflow : you have added too many days and the current
       month cannot handle them any more. The strategy here is to fill the
       current month, and let the next month handle the situation via a
       recursive call. */
    dc_date d1;
    /* We warp to the first day of the next month! */
    dc_add_months(&d1, d, 1);
    /* Now we compute how many days we still have left to add. Because we have
       warped to the next month, we already have added the rest of the days in
       the current month: [days_in_d_month - d.day]. But then we switch
       months, and that corresponds to adding another day. */
    add_dates_days(ret, &d1, days - (days_in_d_month - d->day) - 1);
  } else {
    /* this is the easy case: when you add [days], the new day keeps
       being a valid day in the current month. All is good, we simply warp to
       that new date without any further changes. */
    ret->year = d->year;
    ret->month = d->month;
    ret->day = day_num;
  }
}

dc_success dc_add_dates (dc_date *ret, const dc_date_rounding rnd, const dc_date *d, const dc_period *p) {
  dc_success success;
  ret->year = d->year + p->years;
  ret->month = d->month;
  /* NB: at this point, the date may not be correct.
     Rounding is performed after add_months */
  dc_add_months(ret, ret, p->months);
  ret->day = d->day;
  success = dc_round_date(ret, rnd, ret);
  if (success == dc_ok) {
    add_dates_days(ret, ret, p->days);
    return dc_ok;
  } else
    return success;
}

int dc_compare_dates (const dc_date *d1, const dc_date *d2) {
  long int cmp;
  cmp = d1->year - d2->year;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  cmp = d1->month - d2->month;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  cmp = d1->day - d2->day;
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  return 0;
}

/* Respects ISO8601 format. */
void dc_print_date (const dc_date *d) {
  printf("%04ld-%02lu-%02lu", d->year, d->month, d->day);
}

dc_success dc_date_of_string (dc_date *ret, const char* s) {
  if (sscanf(s, "%4ld-%2lu-%2lu",
             &ret->year, &ret->month, &ret->day)
      == 3)
    return dc_ok;
  else
    return dc_error;
}

void dc_first_day_of_month (dc_date *ret, const dc_date *d) {
  assert(dc_is_valid_date(d));
  ret->year = d->year;
  ret->month = d->month;
  ret->day = 1;
}

void dc_last_day_of_month (dc_date *ret, const dc_date *d) {
  assert(dc_is_valid_date(d));
  ret->year = d->year;
  ret->month = d->month;
  ret->day = dc_days_in_month(d);
}

void dc_neg_period (dc_period *ret, const dc_period *p) {
  ret->years = - p->years;
  ret->months = - p->months;
  ret->days = - p->days;
}

/* The returned [period] is always expressed as a number of days. */
void dc_sub_dates (dc_period *ret, const dc_date *d1, const dc_date *d2) {
  ret->years = 0;
  ret->months = 0;
  if (d1->year == d2->year && d1->month == d2->month) {
    /* Easy case: the two dates are in the same month. */
    ret->days = d1->day - d2->day;
  } else if (dc_compare_dates(d1, d2) < 0) {
    /* The case were d1 is after d2 is symmetrical so we handle it via a
       recursive call changing the order of the arguments. */
    dc_sub_dates(ret, d2, d1);
    dc_neg_period(ret, ret);
  } else { /* d1 > d2 : */
    /* We warp d2 to the first day of the next month. */
    dc_date d2x;
    dc_add_months(&d2x, d2, 1);
    /* Next we divide the result between the number of days we've added to go
       to the end of the month, and the remaining handled by a recursive
       call. */
    dc_sub_dates(ret, d1, &d2x);
    /* The number of days is the difference between the last day of the
       month and the current day of d1, plus one day because we go to
       the next month. */
    ret->days += dc_days_in_month(d2) - d2->day + 1;
  }
}

long int dc_date_year(const dc_date *d) {
  return d->year;
}

unsigned long int dc_date_month(const dc_date *d) {
  return d->month;
}

unsigned long int dc_date_day(const dc_date *d) {
  return d->day;
}

long int dc_period_years(const dc_period *p) {
  return p->years;
}

long int dc_period_months(const dc_period *p) {
  return p->months;
}

long int dc_period_days(const dc_period *p) {
  return p->days;
}

