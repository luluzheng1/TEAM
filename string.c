#include <stdio.h>
#include <stdlib.h>

// Below function extracts characters present in src
// between m and n (excluding n)
char *substring(const char *src, int m, int n)
{
    // get length of the destination string
    int len = n - m;

    // allocate (len + 1) chars for destination (+1 for extra null character)
    char *dest = (char *)malloc(sizeof(char) * (len + 1));

    // extracts characters between m'th and n'th index from source string
    // and copy them into the destination string
    for (int i = m; i < n && (*(src + i) != '\0'); i++)
    {
        *dest = *(src + i);
        dest++;
    }

    // null-terminate the destination string
    *dest = '\0';

    // return the destination string
    return dest - len;
}

#ifdef BUILD_TEST

int main()
{
    char src[] = "Hello World";

    int m = 0;
    int n = 5;

    char *dest = substr(src, m, n);

    printf("%s\n", dest);

    return 0;
}
#endif