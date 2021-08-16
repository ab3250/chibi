#include <stdio.h>
#include <string.h>
#include <time.h>

static void local_date_only(time_t t) {
  char buf[16];
  size_t n;

  n = strftime(buf, sizeof(buf), "%Y-%m-%d", localtime(&t));
  if (n != 10) {
    memset(buf, 0, sizeof(buf));
  }
  printf("%s\n", buf);
}

static void local_date_and_time(time_t t) {
  char buf[32];
  char *zone = &buf[16];
  size_t n;

  n = strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M%z", localtime(&t));
  if (n == 16) {
    snprintf(zone, sizeof(buf) - (zone - buf), "-00:00");
  } else if (n == 21) {
    memmove(&zone[4], &zone[3], sizeof(buf) - (&zone[4] - buf));
    zone[3] = ':';
  } else {
    memset(buf, 0, sizeof(buf));
  }
  printf("%s\n", buf);
}

static void utc_date_and_time(time_t t) {
  char buf[24];
  size_t n;

  n = strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%MZ", gmtime(&t));
  if (n != 17) {
    memset(buf, 0, sizeof(buf));
  }
  printf("%s\n", buf);
}

int main(void) {
  time_t t = time(NULL);

  printf("Local date only:\n");
  local_date_only(t);
  printf("\n");
  printf("Local date and time:\n");
  local_date_and_time(t);
  printf("\n");
  printf("UTC date and time:\n");
  utc_date_and_time(t);
  return 0;
}
