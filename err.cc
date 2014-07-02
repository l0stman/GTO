#include "err.h"

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cstring>

namespace err {
const char *progname;

void
quit(const char *fmt, ...)
{
        va_list ap;

        va_start(ap, fmt);
        fprintf(stderr, "%s: ", progname);
        vfprintf(stderr, fmt, ap);
        va_end(ap);
        putc('\n', stderr);
        exit(EXIT_FAILURE);
}

void
sys(const char *fmt, ...)
{
        va_list ap;

        va_start(ap, fmt);
        fprintf(stderr, "%s: ", progname);
        vfprintf(stderr, fmt, ap);
        va_end(ap);
        fprintf(stderr, ": %s\n", strerror(errno));
        exit(EXIT_FAILURE);
}

void
warn(const char *fmt, ...)
{
        va_list ap;

        va_start(ap, fmt);
        fprintf(stderr, "%s: ", progname);
        vfprintf(stderr, fmt, ap);
        va_end(ap);
        putc('\n', stderr);
}
}
