#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cstring>

#include "err.h"

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
        putchar('\n');
        exit(EXIT_FAILURE);
}

// Fatal error related to a system call.
// Print a message and terminate.
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

// Warn the user by printing a message.
void
warn(const char *fmt, ...)
{
        va_list ap;

        va_start(ap, fmt);
        fprintf(stderr, "%s: ", progname);
        vfprintf(stderr, fmt, ap);
        va_end(ap);
        putchar('\n');
}
}
