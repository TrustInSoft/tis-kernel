
#include <string.h>

void strcat_memcpy(char *dest, const char *s1, int s1size,
                  const char *s2, int s2size)
{
    memcpy(dest, s1, s1size);
    memcpy(dest + s1size, s2, s2size);
}
