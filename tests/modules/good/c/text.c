/* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `catala_error(catala_impossible)` place-holders with
 * your implementation and rename it to remove the ".template" suffix. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <catala_runtime.h>

#include <Stdlib_en.h>
#include <Date_en.h>
#include <List_en.h>
#include <Duration_en.h>
#include <MonthYear_en.h>
#include <Period_en.h>
#include <Money_en.h>
#include <Integer_en.h>
#include <Decimal_en.h>

typedef const char* Text_Text;

int catala_type__Text_Text_equal(const catala_code_position* pos,
                                     const Text_Text t1,
                                     const Text_Text t2) {
  if (strcmp(t1, t2) == 0) return 1;
  else return 0;
}
int catala_type__Text_Text_compare(const catala_code_position* pos,
                                       const Text_Text t1,
                                       const Text_Text t2) {
  int c = strcmp(t1, t2);
  return c > 0 ? 1 : c < 0 ? -1 : 0;
}
void catala_type__Text_Text_print(struct catala_buf buf, const Text_Text t) {
  buf.printf("\"%s\"", t);
}
void catala_type__Text_Text_to_json(struct catala_buf buf, const Text_Text t) {
  int i = 0;
  buf.printf("\"");
  while (1) {
    char c = t[i++];
    switch (c) {
    case '\0': buf.printf("\""); return;
    case '\"': case '\\': buf.printf("\\%c", c); break;
    case '\b': buf.printf("\\b"); break;
    case '\f': buf.printf("\\f"); break;
    case '\n': buf.printf("\\n"); break;
    case '\r': buf.printf("\\r"); break;
    case '\t': buf.printf("\\t"); break;
    default: buf.printf("%c", c);
    }
  }
}

char read_hex_char(const catala_code_position* pos, char c) {
  if ('0' <= c && c <= '9') return c - '0';
  if ('a' <= c && c <= 'f') return 10 + c - 'a';
  if ('A' <= c && c <= 'F') return 10 + c - 'A';
  catala_error(catala_assertion_failed, pos, 1, "Invalid JSON input for type text");
  abort();
}

const Text_Text catala_type__Text_Text_from_json(const catala_code_position* pos, const char * t) {
  int len = strlen(t);
  char* buf;
  int i, ofs;
  if (len < 2 || t[0] != '"' || t[len - 1] != '"')
    catala_error(catala_assertion_failed, pos, 1, "Invalid JSON input for type text");
  buf = catala_malloc (strlen(t) - 2 + 1);
  for (i = 0, ofs = 1; ofs < len - 1; i++, ofs++) {
    switch (t[ofs]) {
    case '"':
      catala_error(catala_assertion_failed, pos, 1, "Invalid JSON input for type text");
    case '\\':
      ofs++;
      if (ofs >= len - 1)
        catala_error(catala_assertion_failed, pos, 1, "Invalid JSON input for type text");
      switch (t[ofs]) {
      case 'b': buf[i] = '\b'; break;
      case 'f': buf[i] = '\f'; break;
      case 'n': buf[i] = '\n'; break;
      case 'r': buf[i] = '\r'; break;
      case 't': buf[i] = '\t'; break;
      case '"': case '/': case '\\':
        buf[i] = t[ofs]; break;
      case 'u':
        if (ofs >= len - 5)
          catala_error(catala_assertion_failed, pos, 1, "Invalid JSON input for type text");
        else {
          char w = read_hex_char(pos, t[++ofs]);
          char x = read_hex_char(pos, t[++ofs]);
          char y = read_hex_char(pos, t[++ofs]);
          char z = read_hex_char(pos, t[++ofs]);
          if (w == 0 && x == 0 && y < 8) { /* 1-byte encoding (ASCII) */
            buf[i] = y << 4 | z;
          }
          else if (x < 8) { /* 2-byte encoding */
            buf[i] = 0xc0 | x << 2 | y >> 2;
            buf[++i] = 0x80 | (y << 6 >> 2) | z;
          }
          else  { /* 3-byte encoding */
            buf[i] = 0xe0 | w;
            buf[++i] = 0x80 | (x << 2) | (y >> 2);
            buf[++i] = 0x80 | (y << 6 >> 2) | z;
          }
        }
        break;
      default:
        catala_error(catala_assertion_failed, pos, 1, "Invalid JSON input for type text");
      }
      break;
    default:
      buf[i] = t[ofs];
    }
  }
  buf[i] = '\0';
  return buf;
}
const catala_type catala_type__Text_Text()
{
  static catala_type ty = {UNINITIALIZED};
  if (ty.kind != UNINITIALIZED) return ty;
  ty.contents.texternal.name = "Text_Text";
  ty.contents.texternal.equal =
    (int (*)(const catala_code_position *, const void *, const void *))
    &catala_type__Text_Text_equal;
  ty.contents.texternal.compare =
    (int (*)(const catala_code_position *, const void *, const void *))
    &catala_type__Text_Text_compare;
  ty.contents.texternal.print =
    (void (*)(struct catala_buf, const void *))
    &catala_type__Text_Text_print;
  ty.contents.texternal.to_json =
    (void (*)(struct catala_buf, const void *))
    &catala_type__Text_Text_to_json;
  ty.contents.texternal.from_json =
    (void * (*)(const catala_code_position *, const char *))
    &catala_type__Text_Text_from_json;
  ty.kind = EXTERNAL;
  return ty;
}

const catala_code_position Text__loc[][1] = {
  {{"tests/modules/good/text.catala_en", 8, 13, 8, 16}},
  {{"tests/modules/good/text.catala_en", 9, 13, 9, 16}},
  {{"tests/modules/good/text.catala_en", 10, 13, 10, 21}},
  {{"tests/modules/good/text.catala_en", 11, 13, 11, 19}}
};

const Text_Text Text__foo () {
  return "foo\\";
}

const Text_Text Text__bar () {
  return "bąr";
}

const Text_Text Text__fortytwo () {
  return "42";
}

const Text_Text Text__of_int (CATALA_INT x)
{
  catala_print(catala_strbuf, embed(catala_type_integer, x));
  return catala_strbuf.flush();
}
