#ifndef ERR_H_
#define ERR_H_

namespace err {
// Name of the running program.
extern const char *progname;

// The following functions use printf-like formatting.

// Fatal error non-related to a system call.
// Print a message and terminate.
extern void quit(const char *fmt, ...);

// Fatal error related to a system call.
// Print a message and terminate.
extern void sys(const char *fmt, ...);

// Warn the user by printing a message.
extern void warn(const char *fmt, ...);
} // namespace err

#endif  // !ERR_H_
