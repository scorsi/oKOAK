#include <stdio.h>

/* put_double - printf that takes a double and returns 0. */
extern double put_double(double a) {
    printf("%e", a);
    return 0;
}

/* put_int - printf that takes an int and returns 0. */
extern int put_int(int a) {
    printf("%d", a);
    return 0;
}

/* put_char - printf that takes a char and returns 0. */
extern char put_char(char a) {
    printf("%c", a);
    return 0;
}
