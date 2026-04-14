/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#ifndef __TEXT_H__
#define __TEXT_H__

#include <Stdlib_en.h>
#include <Date_en.h>
#include <List_en.h>
#include <Duration_en.h>
#include <MonthYear_en.h>
#include <Period_en.h>
#include <Money_en.h>
#include <Integer_en.h>
#include <Decimal_en.h>
typedef char* Text_Text;

const catala_type catala_type__Text_Text();

const Text_Text* Text__foo ();

const Text_Text* Text__bar ();

const Text_Text* Text__fortytwo ();

const Text_Text* Text__of_int (CATALA_INT x);

#endif /* __TEXT_H__ */
