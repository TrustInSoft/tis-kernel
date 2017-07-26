
void simple_write(const char *src, char *dest, int size)
{
    while(size && *src) {
        *dest = *src;
        size--;
        dest++;
        src++;
    }
}

int main(void)
{
    char buffer[8];

    simple_write("A long string", buffer, 64);

    return 0;
}
