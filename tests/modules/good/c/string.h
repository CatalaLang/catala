/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#ifndef __STRING_H__
#define __STRING_H__

#include <Stdlib_en.h>
#include <Date_en.h>
#include <List_en.h>
#include <Duration_en.h>
#include <MonthYear_en.h>
#include <Period_en.h>
#include <Money_en.h>
#include <Integer_en.h>
#include <Decimal_en.h>
typedef char* String_String;

const catala_type catala_type__String_String();

const String_String* String__foo ();

const String_String* String__bar ();

const String_String* String__fortytwo ();

const String_String* String__of_int (CATALA_INT x);

#endif /* __STRING_H__ */
