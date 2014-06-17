#ifndef ERR_H_
#define ERR_H_

namespace err {
extern const char *progname;
extern void quit(const char *fmt, ...);
extern void sys(const char *fmt, ...);
extern void warn(const char *fmt, ...);
}

#endif  // !ERR_H_
