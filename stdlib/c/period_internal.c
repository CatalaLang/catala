#include <stdlib.h>
#include <assert.h>
#include <catala_runtime.h>

CATALA_DATE get_date_in_arr(const CATALA_ARRAY(CATALA_TUPLE(CATALA_TUPLE(CATALA_DATE; CATALA_DATE), void *)) arr, const int i){
  CATALA_TUPLE(_) t;
  CATALA_TUPLE(_) t2;

  t = ((CATALA_TUPLE(_))(arr->elements[i]));
  t2 =((CATALA_TUPLE(_))(t[0].content));
  return ((CATALA_DATE)(t2[0].content));
}

int leq_period(CATALA_DATE ld, CATALA_DATE rd){
  return dc_compare_dates(ld, rd) <= 0;
}

void merge(CATALA_ARRAY(CATALA_TUPLE(CATALA_TUPLE(CATALA_DATE; CATALA_DATE), void *)) arr, int start, int mid, int end)
{
  CATALA_DATE tmp;
  const void * value;
  int index;
  int start2 = mid + 1;
  if (leq_period(get_date_in_arr(arr, mid), get_date_in_arr(arr, start2)))
    {
      return;
    }
  while (start <= mid && start2 <= end)
    {
      if (leq_period(get_date_in_arr(arr,start), get_date_in_arr(arr, start2)))
        {
          start++;
        }
      else
        {
          value = arr->elements[start2];
          index = start2;
          tmp = get_date_in_arr(arr, start2);
          while (index != start)
            {
              arr->elements[index] = arr->elements[index - 1];
              index--;
            }
          arr->elements[start] = value;
          start++;
          mid++;
          start2++;
        }
    }
}

void mergeSort(CATALA_ARRAY(CATALA_TUPLE(CATALA_TUPLE(CATALA_DATE; CATALA_DATE), void *)) arr, int l, int r)
{
  int m;
  if (l < r)
    {
      m = l + (r - l) / 2;
      mergeSort(arr, l, m);
      mergeSort(arr, m + 1, r);
      merge(arr, l, m, r);
    }
}

CATALA_ARRAY(CATALA_TUPLE(CATALA_TUPLE(CATALA_DATE; CATALA_DATE), void *))
  PeriodInternal__sort(const CATALA_ARRAY(CATALA_TUPLE(CATALA_TUPLE(CATALA_DATE; CATALA_DATE), void *)) p)
{
  int i;
  catala_array *ret = catala_malloc(sizeof(catala_array));
  ret->size = 0;
  ret->elements = catala_malloc(p->size * sizeof(void *));
  for (i = 0; i < p->size; i++)
    {
      ret->elements[ret->size++] = p->elements[i];
    }
  mergeSort(ret, 0, p->size - 1);
  return ret;
}

CATALA_TUPLE(_) mk_period(CATALA_DATE d1, CATALA_DATE d2){
  CATALA_TUPLE(_) t = catala_malloc (2 * sizeof(tuple_element));
  t[0].content = d1;
  t[1].content = d2;
  return t;
}

const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE; CATALA_DATE))
PeriodInternal__split_by_month(const CATALA_TUPLE(CATALA_DATE; CATALA_DATE) p)
{
  CATALA_DATE start = p[0].content;
  CATALA_DATE stop = p[1].content;
  int estimated_size;
  CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE; CATALA_DATE)) ret;
  dc_date * next_end;
  dc_date * next;
  dc_date tmp;
  dc_period one_month;
  dc_period m_one_day;
  char should_continue;
  int idx;

  ret = catala_malloc(sizeof(catala_array));

  /* stop should be after start */
  if (dc_compare_dates(start, stop) >= 0){
    ret->size = 0;
    ret->elements = NULL;
    return ret;
  }

  estimated_size = (dc_date_year(stop) - dc_date_year(start) + 1) * 12;
  ret->elements = catala_malloc(estimated_size * sizeof(CATALA_TUPLE(_)));

  dc_make_period(&one_month, 0, 1, 0);
  dc_make_period(&m_one_day, 0, 0, -1);
  idx = 0;

  while (1) {
    next = catala_malloc(sizeof(dc_date));
    dc_first_day_of_month(&tmp, start);
    dc_add_dates(next, dc_date_round_abort, &tmp, &one_month);

    if (dc_compare_dates(next, stop) < 0){
      next_end = catala_malloc(sizeof(dc_date));
      dc_add_dates(next_end, dc_date_round_abort, next, &m_one_day);
      ret->elements[idx++] = mk_period(start, next_end);
      start = next;
      continue;
    }
    if (dc_compare_dates(start , stop) < 0){
      ret->elements[idx++] = mk_period(start , stop);
    }
    break;
  }
  ret->size = idx;

  return ret;
}

void first_day_of_rolling_year(dc_date * ret_date, CATALA_DATE date, CATALA_INT start_month){
  long int year;
  unsigned long int month;
  unsigned long int start_month_ui = mpz_get_ui(start_month);
  year = dc_date_year(date);
  month = dc_date_month(date);
  if (month < start_month_ui)
    year--;
  dc_make_date(ret_date, year, start_month_ui, 1);
}

const CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE; CATALA_DATE))
PeriodInternal__split_by_year(CATALA_INT start_month, const CATALA_TUPLE(CATALA_DATE; CATALA_DATE) p)
{
  CATALA_DATE start = p[0].content;
  CATALA_DATE stop = p[1].content;
  int estimated_size;
  CATALA_ARRAY(CATALA_TUPLE(CATALA_DATE; CATALA_DATE)) ret;
  dc_date * next;
  dc_date * next_end;
  dc_date tmp;
  dc_period one_year;
  dc_period m_one_day;
  char should_continue;
  int idx;

  assert(mpz_get_si(start_month) >= 1 && mpz_get_si(start_month) <= 12);

  ret = catala_malloc(sizeof(catala_array));

  /* stop should be after start */
  if (dc_compare_dates(start, stop) >= 0){
    ret->size = 0;
    ret->elements = NULL;
    return ret;
  }

  estimated_size = (dc_date_year(stop) - dc_date_year(start) + 1);
  ret->elements = catala_malloc(estimated_size * sizeof(CATALA_TUPLE(_)));
  dc_make_period(&one_year, 1, 0, 0);
  dc_make_period(&m_one_day, 0, 0, -1);
  idx = 0;

  while (1) {
    next = catala_malloc(sizeof(dc_date));
    first_day_of_rolling_year(&tmp, start, start_month);
    dc_add_dates(next, dc_date_round_abort, &tmp, &one_year);

    if (dc_compare_dates(next, stop) < 0){
      next_end = catala_malloc(sizeof(dc_date));
      dc_add_dates(next_end, dc_date_round_abort, next, &m_one_day);
      ret->elements[idx++] = mk_period(start, next_end);
      start = next;
      continue;
    }
    if (dc_compare_dates(start , stop) < 0){
      ret->elements[idx++] = mk_period(start , stop);
    }
    break;
  }
  ret->size = idx;
  return ret;
}
