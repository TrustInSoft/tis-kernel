/**************************************************************************/
/*                                                                        */
/*  This file is part of tis-interpreter.                                 */
/*                                                                        */
/*    Copyright (C) 2016-2017 TrustInSoft                                 */
/*                                                                        */
/*  tis-interpreter is released under GPLv2                               */
/*                                                                        */
/**************************************************************************/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <alloca.h>

#define __KEY_SIZE 64
#define __VALUE_SIZE 128
#define __MAX_ENV 128

static int _env_pos = 0;
struct _env {
    char key[__KEY_SIZE];
    char value[__VALUE_SIZE];
};
static struct _env _env[__MAX_ENV];

char *_strncpy(char *dest, const char *src, size_t n)
{
    size_t src_size, s;

    src_size = strlen(src);

    s = (src_size > n ? n : src_size);
    memcpy(dest, src, s);

    if (n > s)
        memset(dest + s, '\0', n - s);

    return dest;
}

int setenv(const char *name, const char *value, int overwrite)
{
    int i;

    if (!name || name[0] == '\0' || strchr(name, '=')) {
        errno = EINVAL;
        return -1;
    }

    for (i = 0; i < _env_pos; i++) {
        if (strncmp(_env[i].key, name, __KEY_SIZE) == 0) {
            if (overwrite) {
                if (value)
                    _strncpy(_env[i].value, value, __VALUE_SIZE);
                else
                    _env[i].value[0] = '\0';
            }
            return 0;
        }
    }

    if (_env_pos >= __MAX_ENV) {
        errno = ENOMEM;
        return -1;
    } else {
        _strncpy(_env[_env_pos].key, name, __KEY_SIZE);

        if (value)
            _strncpy(_env[_env_pos].value, value, __VALUE_SIZE);
        else
            _env[_env_pos].value[0] = '\0';

        _env_pos++;
    }

    return 0;
}

int unsetenv(const char *name)
{
    int i;

    if (!name || name[0] == '\0' || strchr(name, '=')) {
        errno = EINVAL;
        return -1;
    }

    for (i = 0; i < _env_pos; i++) {
        if (strncmp(_env[i].key, name, __KEY_SIZE) == 0) {
            if (i == _env_pos - 1) {
                /* The variable is the last one in the array */
                _env[i].key[0] = '\0';
                _env[i].value[0] = '\0';
            }
            else {
                /* If it's not the last one, just copy the last into the
                   case. */
                _strncpy(_env[_env_pos - 1].key, _env[i].key, __KEY_SIZE);
                _strncpy(_env[_env_pos - 1].value, _env[i].value, __VALUE_SIZE);
            }

            --_env_pos;
            break;
        }
    }

    return 0;
}

char *getenv(const char *name)
{
    int i;

    if (!name)
        return NULL;

    for (i = 0; i < _env_pos; i++) {
        if (strncmp(_env[i].key, name, __KEY_SIZE) == 0) {
            return _env[i].value;
        }
    }

    return NULL;
}

int clearenv(void)
{
    int i;
    for (i = 0; i < _env_pos; i++) {
        _env[i].key[0]   = '\0';
        _env[i].value[0] = '\0';
    }

    _env_pos = 0;
    return 0;
}
