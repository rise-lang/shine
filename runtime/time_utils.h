#include <time.h>

typedef struct timespec Instant;

inline Instant now() {
  Instant i;
  clock_gettime(CLOCK_MONOTONIC, &i);
  return i;
}

inline double elapsedSeconds(Instant a, Instant b) {
  long seconds = b.tv_sec - a.tv_sec;
  long nanoseconds = b.tv_nsec - a.tv_nsec;
  return seconds + nanoseconds*1e-9;
}