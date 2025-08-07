#include <stdio.h>
#include <stdlib.h>
#include <catala_runtime.h>


CATALA_DATE DateInternal__of_ymd
    (CATALA_INT dyear, CATALA_INT dmonth, CATALA_INT dday)
{
  static const catala_code_position pos[1] =
    {{"stdlib/date_internal.catala_en", 8, 10, 8, 20}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

const CATALA_TUPLE(CATALA_INT;CATALA_INT;CATALA_INT) DateInternal__to_ymd
    (CATALA_DATE d)
{
  static const catala_code_position pos[1] =
    {{"stdlib/date_internal.catala_en", 12, 10, 12, 20}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

CATALA_DATE DateInternal__last_day_of_month (CATALA_DATE d)
{
  static const catala_code_position pos[1] =
    {{"stdlib/date_internal.catala_en", 16, 10, 16, 20}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

