#ifndef INPUT_H_
#define INPUT_H_

namespace input {

// Wrapper around "scanf" that exits on error.
extern void ScanfOrDie(const char *fmt, ...);

// Return a string on the next non-empty line read from "stdin" and
// drop the newline at the end of a line. Exit on error.  The function
// returns a pointer to a fixed-size internal storage space that will
// be overwritten by subsequent calls.
extern char *ReadLineOrDie(void);

} // namespace input

#endif  // !INPUT_H_
