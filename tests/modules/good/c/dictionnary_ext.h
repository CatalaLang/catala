/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#ifndef __DICTIONNARY_EXT_H__
#define __DICTIONNARY_EXT_H__

#include <Stdlib_en.h>
#include <Date_en.h>
#include <MonthYear_en.h>
#include <Period_en.h>
#include <Money_en.h>
#include <Integer_en.h>
#include <Decimal_en.h>
#include <List_en.h>
typedef struct dict {
  CATALA_INT key;
  CATALA_MONEY value;
  struct dict * rest;
} Dictionnary_ext_Dictionnary;

const Dictionnary_ext_Dictionnary* DictionnaryExt__empty ();

const Dictionnary_ext_Dictionnary* DictionnaryExt__store
    (const Dictionnary_ext_Dictionnary* dict,
     CATALA_INT key,
     CATALA_MONEY value);

const CATALA_OPTION(CATALA_MONEY) DictionnaryExt__find
    (const Dictionnary_ext_Dictionnary* dict, CATALA_INT key);

#endif /* __DICTIONNARY_EXT_H__ */
