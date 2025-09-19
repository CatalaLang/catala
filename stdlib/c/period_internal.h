/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#ifndef __PERIOD_INTERNAL_H__
#define __PERIOD_INTERNAL_H__


const CATALA_ARRAY(CATALA_TUPLE(CATALA_TUPLE(CATALA_DATE;CATALA_DATE), void *))
    PeriodInternal__sort
    (const CATALA_ARRAY(CATALA_TUPLE(CATALA_TUPLE(CATALA_DATE;CATALA_DATE), void *)) p);

const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE;CATALA_DATE))
    PeriodInternal__split_by_month
    (const CATALA_TUPLE(CATALA_DATE;CATALA_DATE) p);

const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE;CATALA_DATE))
    PeriodInternal__split_by_year
    (CATALA_INT start_month, const CATALA_TUPLE(CATALA_DATE;CATALA_DATE) p);

#endif /* __PERIOD_INTERNAL_H__ */
