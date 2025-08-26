/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#include <stdio.h>
#include <stdlib.h>
#include <catala_runtime.h>


const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE;CATALA_DATE))
    PeriodInternal__sort
    (const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE;CATALA_DATE)) p)
{
  static const catala_code_position pos[1] =
    {{"stdlib/period_internal.catala_en", 6, 13, 6, 17}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE;CATALA_DATE))
    PeriodInternal__split_by_month
    (const CATALA_TUPLE(CATALA_DATE;CATALA_DATE) p)
{
  static const catala_code_position pos[1] =
    {{"stdlib/period_internal.catala_en", 9, 13, 9, 27}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE;CATALA_DATE))
    PeriodInternal__split_by_year
    (CATALA_INT start_month, const CATALA_TUPLE(CATALA_DATE;CATALA_DATE) p)
{
  static const catala_code_position pos[1] =
    {{"stdlib/period_internal.catala_en", 12, 13, 12, 26}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

