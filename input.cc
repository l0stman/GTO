#include "input.h"

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "err.h"

namespace input {

void
ScanfOrDie(const char *fmt, ...)
{
        va_list ap;
        int ret;

        va_start(ap, fmt);
        if ((ret = vscanf(fmt, ap)) == EOF)
                exit(EXIT_FAILURE);
        else if (ret == 0)
                err::quit("ScanfOrDie input error.");
        va_end(ap);
}

char *
ReadLineOrDie(void)
{
        static char buf[BUFSIZ];
        for (;;) {
                if (fgets(buf, sizeof(buf), stdin) == NULL) {
                        if (feof(stdin))
                                exit(EXIT_FAILURE);
                        else
                                err::sys("ReadLineOrDie");
                }
                size_t len = strlen(buf)-1;
                if (len == 0)
                        continue; // empty line
                if (buf[len] == '\n') {
                        buf[len] = '\0';
                        break;
                }

        }
        return buf;
}

} // namespace input
