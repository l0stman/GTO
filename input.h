#ifndef INPUT_H_
#define INPUT_H_

namespace input {

// Wrapper around "scanf" that exits on error.
extern void ScanfOrDie(const char *fmt, ...);

// Return a string on the next non-empty line read from "stdin" and
// drop the newline. Exit on error.
extern char *ReadLineOrDie(void);

} // namespace input

#endif  // !INPUT_H_
