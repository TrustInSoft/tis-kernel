
#include <stdio.h>

#include "concat.h"

int main(void)
{
    char buffer[32];

    strcat_memcpy(buffer, "abc", 3, "defgh", 5);
    printf("%.*s", 32, buffer);

    return 0;
}
