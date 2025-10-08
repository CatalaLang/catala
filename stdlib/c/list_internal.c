/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#include <stdio.h>
#include <stdlib.h>
#include <catala_runtime.h>


const CATALA_ARRAY(CATALA_INT) ListInternal__sequence
    (CATALA_INT begin, CATALA_INT end)
{
  static const catala_code_position pos[1] =
    {{"stdlib/list_internal.catala_en", 4, 13, 4, 21}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

const CATALA_OPTION(void * /* any t */) ListInternal__nth_element
    (const CATALA_ARRAY(void * /* any t */) lst, CATALA_INT index)
{
  static const catala_code_position pos[1] =
    {{"stdlib/list_internal.catala_en", 9, 13, 9, 24}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

const CATALA_ARRAY(void * /* any t */) ListInternal__remove_nth_element
    (const CATALA_ARRAY(void * /* any t */) lst, CATALA_INT index)
{
  static const catala_code_position pos[1] =
    {{"stdlib/list_internal.catala_en", 14, 13, 14, 31}};
  catala_error(catala_impossible, pos, 1);
  abort();
}

