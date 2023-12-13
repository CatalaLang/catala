#include "simple.c"

int main()
{

    if (!setjmp(catala_fatal_error_jump_buffer))
    {

        baz_in_struct input = {
            {bar_enum_no_cons, {no_cons : NULL}}};
        baz_struct output = baz_func(input);
        printf("Output: %f\n", output.b_field);
        return 0;
    }
    else
    {
        char *error_kind;
        switch (catala_fatal_error_raised.code)
        {
        case catala_no_value_provided:
            error_kind = "No value provided";
            break;
        case catala_conflict:
            error_kind = "Conflict between exceptions";
            break;
        case catala_crash:
            error_kind = "Crash";
            break;
        case catala_empty:
            error_kind = "Empty error not caught";
            break;
        }
        printf("\033[1;31m[ERROR]\033[0m %s in file %s:%d.%d-%d.%d\n",
               error_kind,
               catala_fatal_error_raised.position.filename,
               catala_fatal_error_raised.position.start_line,
               catala_fatal_error_raised.position.start_column,
               catala_fatal_error_raised.position.end_line,
               catala_fatal_error_raised.position.end_column);
        return -1;
    }
}