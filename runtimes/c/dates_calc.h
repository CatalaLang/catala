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

#ifndef __DATES_CALC_H__
#define __DATES_CALC_H__

typedef enum dc_success {
  dc_error, dc_ok
} dc_success;


/* It is expected for [dc_date] and [dc_period] to be stack-allocated by the
   caller, so although we don't expose their contents, we provide [_opaque_data]
   as a hint about their sizes to the compiler. */
typedef struct dc_date { long int _opaque_data[3]; } dc_date;

typedef struct dc_period { long int _opaque_data[3]; } dc_period;

typedef enum dc_date_rounding {
  dc_date_round_up,
  dc_date_round_down,
  dc_date_round_abort
} dc_date_rounding;

dc_success dc_make_date(dc_date *ret, const long int y, const unsigned long int m, const unsigned long int d);

dc_success dc_add_dates (dc_date *ret, const dc_date_rounding rnd, const dc_date *d, const dc_period *p);

void dc_sub_dates (dc_period *ret, const dc_date *d1, const dc_date *d2);

int dc_compare_dates (const dc_date *d1, const dc_date *d2);

long int dc_date_year(const dc_date *d);
unsigned long int dc_date_month(const dc_date *d);
unsigned long int dc_date_day(const dc_date *d);

void dc_print_date (const dc_date *d);

dc_success dc_date_of_string (dc_date *ret, const char* s);

void dc_first_day_of_month (dc_date *ret, const dc_date *d);

void dc_last_day_of_month (dc_date *ret, const dc_date *d);

int dc_is_leap_year (const long int y);

void dc_make_period(dc_period *ret, const long int y, const long int m, const long int d);
void dc_neg_period (dc_period *ret, const dc_period *p);
void dc_add_periods (dc_period *ret, const dc_period *p1, const dc_period *p2);
void dc_sub_periods (dc_period *ret, const dc_period *p1, const dc_period *p2);
void dc_mul_periods (dc_period *ret, const dc_period *p, const long int m);

void dc_print_period (const dc_period *p);
dc_success dc_period_of_string (dc_period *ret, const char* s);

dc_success dc_period_to_days (long int *ret, const dc_period *p);

long int dc_period_years(const dc_period *d);
long int dc_period_months(const dc_period *d);
long int dc_period_days(const dc_period *d);

#endif /* __DATES_CALC_H */
