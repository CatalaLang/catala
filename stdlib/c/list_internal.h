/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#ifndef __LIST_INTERNAL_H__
#define __LIST_INTERNAL_H__


const CATALA_ARRAY(CATALA_INT) ListInternal__sequence
    (CATALA_INT begin, CATALA_INT end);

const CATALA_OPTION(void * /* any t */) ListInternal__nth_element
    (const CATALA_ARRAY(void * /* any t */) lst, CATALA_INT index);

const CATALA_ARRAY(void * /* any t */) ListInternal__remove_nth_element
    (const CATALA_ARRAY(void * /* any t */) lst, CATALA_INT index);

#endif /* __LIST_INTERNAL_H__ */
