#include <stdio.h>
#include <stdlib.h>
#include <catala_runtime.h>


CATALA_DATE DateInternal__of_ymd
    (CATALA_POSITION pos,
     CATALA_INT dyear,
     CATALA_INT dmonth,
     CATALA_INT dday)
{
  if (mpz_fits_slong_p(dyear) && mpz_fits_ulong_p(dmonth) && mpz_fits_ulong_p(dday)) {
    dc_date *ret = catala_malloc(sizeof(dc_date));
    const long int y = mpz_get_si(dyear);
    const unsigned long int m = mpz_get_ui(dmonth);
    const unsigned long int d = mpz_get_ui(dday);
    const int success = dc_make_date(ret, y, m, d);
    if (success) return ret;
  }
  catala_error(catala_uncomparable_durations, pos, 1);
  abort();
}

const CATALA_TUPLE(CATALA_INT;CATALA_INT;CATALA_INT) DateInternal__to_ymd
    (CATALA_DATE d)
{
  CATALA_TUPLE(CATALA_INT;CATALA_INT;CATALA_INT) ret =
    catala_malloc(3 * sizeof(tuple_element));
  ret[0].content = catala_new_int(dc_date_year(d));
  ret[1].content = catala_new_int(dc_date_month(d));
  ret[2].content = catala_new_int(dc_date_day(d));
  return ret;
}

CATALA_DATE DateInternal__last_day_of_month (CATALA_DATE d)
{
  dc_date *ret = catala_malloc(sizeof(dc_date));
  dc_last_day_of_month(ret, d);
  return ret;
}

