#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <dates_calc.h>

unsigned long int test_add_dates_exact()
{
  const char* fname = "exact_computations.csv";
  FILE* f = fopen(fname, "r");
  char* readbuf = NULL;
  size_t readbuflen = 0;
  int failed = 0;
  int total = 0;
  if (f == NULL) {
    fprintf(stderr, "Could not open file %s\n", fname);
    abort();
  }
  getline(&readbuf, &readbuflen, f);
  while (getline(&readbuf, &readbuflen, f) >= 0)
    {
      char *s_t, *s_dt, *s_expect;
      dc_date t, expect, result;
      dc_period dt;
      total++;
      if (sscanf(readbuf, "%m[^;];%m[^;];%m[^;]", &s_t, &s_dt, &s_expect) < 3) {
        fprintf(stderr, "Error: Bad test %s", readbuf);
        failed++;
        continue;
      }
      dc_date_of_string(&t, s_t);
      dc_period_of_string(&dt, s_dt);
      dc_date_of_string(&expect, s_expect);
      dc_add_dates(&result, dc_date_round_abort, &t, &dt);
      if (dc_compare_dates (&result, &expect) != 0) {
        printf("Test FAILED: %s", readbuf);
        printf("    ");
        dc_print_date(&result);
        printf(" != ");
        dc_print_date(&expect);
        printf("\n");
        failed++;
      }
      free(s_t);
      free(s_dt);
      free(s_expect);
    }
  free(readbuf);
  fclose(f);
  if (errno) {
    fprintf(stderr, "Could not read file %s\n", fname);
    abort();
  }
  return ((total << 16) | failed);
}

unsigned long int test_add_dates_ambiguous()
{
  const char* fname = "ambiguous_computations.csv";
  FILE* f = fopen(fname, "r");
  char* readbuf = NULL;
  size_t readbuflen = 0;
  int failed = 0;
  int total = 0;
  if (f == NULL) {
    fprintf(stderr, "Could not open file %s\n", fname);
    abort();
  }
  getline(&readbuf, &readbuflen, f);
  while (getline(&readbuf, &readbuflen, f) >= 0)
    {
      char *s_t, *s_dt, *s_expect_up, *s_expect_down;
      dc_date t, expect_up, expect_down, result_up, result_down;
      dc_period dt;
      total++;
      if (sscanf(readbuf, "%m[^;];%m[^;];%m[^;];%m[^;]",
                 &s_t, &s_dt, &s_expect_up, &s_expect_down)
          < 4)
        {
          fprintf(stderr, "Error: Bad test %s", readbuf);
          failed++;
          continue;
        }
      dc_date_of_string(&t, s_t);
      dc_period_of_string(&dt, s_dt);
      dc_date_of_string(&expect_up, s_expect_up);
      dc_date_of_string(&expect_down, s_expect_down);
      dc_add_dates(&result_up, dc_date_round_up, &t, &dt);
      dc_add_dates(&result_down, dc_date_round_down, &t, &dt);
      if (dc_compare_dates (&result_up, &expect_up) != 0) {
        printf("Test FAILED: %s", readbuf);
        printf("    ");
        dc_print_date(&result_up);
        printf(" != ");
        dc_print_date(&expect_up);
        printf("\n");
        failed++;
      }
      else if (dc_compare_dates (&result_down, &expect_down) != 0) {
        printf("Test FAILED: %s", readbuf);
        printf("    ");
        dc_print_date(&result_down);
        printf(" != ");
        dc_print_date(&expect_down);
        printf("\n");
        failed++;
      }
      free(s_t);
      free(s_dt);
      free(s_expect_up);
      free(s_expect_down);
    }
  free(readbuf);
  fclose(f);
  if (errno) {
    fprintf(stderr, "Could not read file %s\n", fname);
    abort();
  }
  return ((total << 16) | failed);
}

unsigned long int test_first_last_day_of_month()
{
  const char* fname = "first_last_day_of_month.csv";
  FILE* f = fopen(fname, "r");
  char* readbuf = NULL;
  size_t readbuflen = 0;
  int failed = 0;
  int total = 0;
  if (f == NULL) {
    fprintf(stderr, "Could not open file %s\n", fname);
    abort();
  }
  getline(&readbuf, &readbuflen, f);
  while (getline(&readbuf, &readbuflen, f) >= 0)
    {
      char *s_t, *s_expect_first, *s_expect_last;
      dc_date t, expect_first, expect_last, result_first, result_last;
      total++;
      if (sscanf(readbuf, "%m[^;];%m[^;];%m[^;]",
                 &s_t, &s_expect_first, &s_expect_last)
          < 3)
        {
          fprintf(stderr, "Error: Bad test %s", readbuf);
          failed++;
          continue;
        }
      dc_date_of_string(&t, s_t);
      dc_date_of_string(&expect_first, s_expect_first);
      dc_date_of_string(&expect_last, s_expect_last);
      dc_first_day_of_month(&result_first, &t);
      dc_last_day_of_month(&result_last, &t);
      if (dc_compare_dates (&result_first, &expect_first) != 0) {
        printf("Test FAILED: %s", readbuf);
        printf("    ");
        dc_print_date(&result_first);
        printf(" != ");
        dc_print_date(&expect_first);
        printf("\n");
        failed++;
      }
      else if (dc_compare_dates (&result_last, &expect_last) != 0) {
        printf("Test FAILED: %s", readbuf);
        printf("    ");
        dc_print_date(&result_last);
        printf(" != ");
        dc_print_date(&expect_last);
        printf("\n");
        failed++;
      }
      free(s_t);
      free(s_expect_first);
      free(s_expect_last);
    }
  free(readbuf);
  fclose(f);
  if (errno) {
    fprintf(stderr, "Could not read file %s\n", fname);
    abort();
  }
  return ((total << 16) | failed);
}

int main()
{
  unsigned long int results = 0;
  int total = 0;
  int failed = 0;
  results += test_add_dates_exact();
  results += test_add_dates_ambiguous();
  results += test_first_last_day_of_month();
  failed = results & 0xFFFF;
  total = results >> 16;
  if (failed > 0) {
    printf("=== C Tests \x1b[31mFAILED\x1b[m ===\n");
    printf("Tests failed: \x1b[31m%d\x1b[m\n", failed);
    printf("Tests passed: %d / %d\n", total - failed, total);
    return 1;
  }
  else {
    printf("=== C Tests \x1b[32mPASSED\x1b[m ===\n");
    printf("Tests passed: %d / %d\n", total, total);
    return 0;
  }
}
