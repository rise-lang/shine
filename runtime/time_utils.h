#include <time.h>

typedef struct timespec Instant;

inline void assertReasonableTimeResolution() {
  struct timespec res;
  if (clock_getres(CLOCK_MONOTONIC, &res) != 0) {
    fprintf(stderr, "could not get clock resolution\n");
    exit(EXIT_FAILURE);
  }
  if (res.tv_sec > 0 || res.tv_nsec >= 1000) {
    fprintf(stderr, "clock resolution was lower than 1µs\n");
    exit(EXIT_FAILURE);
  }
}

inline Instant now() {
  Instant i;
  if (clock_gettime(CLOCK_MONOTONIC, &i) != 0) {
    fprintf(stderr, "could not get clock time\n");
    exit(EXIT_FAILURE);
  }
  return i;
}

inline double elapsedSeconds(Instant a, Instant b) {
  long seconds = b.tv_sec - a.tv_sec;
  long nanoseconds = b.tv_nsec - a.tv_nsec;
  return seconds + nanoseconds*1e-9;
}