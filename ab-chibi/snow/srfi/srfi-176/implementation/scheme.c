#ifdef SCHEME_UNIX
#include <sys/stat.h>
#include <sys/utsname.h>
#include <unistd.h>
#endif

#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "scheme_env.h"

// How many bits wide is the given C type?
#define BITSIZEOF(t) (sizeof(t) * CHAR_BIT)

// Every Scheme implementation has a value type like this

#define T_PAIR 1
#define T_INTEGER 2
#define T_SYMBOL 3
#define T_STRING 4

struct value {
  uintptr_t type;
  union {
    uintptr_t integer;
    const char *string;
    struct {
      struct value *car;
      struct value *cdr;
    } pair;
  } v;
};

static struct value *make_value(uintptr_t type) {
  struct value *value;

  value = calloc(1, sizeof(struct value));
  value->type = type;
  return value;
}

static struct value *integer(uintptr_t u) {
  struct value *value;

  value = make_value(T_INTEGER);
  value->v.integer = u;
  return value;
}

static struct value *symbol(const char *s) {
  struct value *value;

  value = make_value(T_SYMBOL);
  value->v.string = s;
  return value;
}

static struct value *string(const char *s) {
  struct value *value;

  if (!s) {
    return 0;
  }
  value = make_value(T_STRING);
  value->v.string = s;
  return value;
}

static struct value *pair(struct value *car, struct value *cdr) {
  struct value *value;

  value = make_value(T_PAIR);
  value->v.pair.car = car;
  value->v.pair.cdr = cdr;
  return value;
}

// Convenience functions to build lists more easily

static struct value *list2(struct value *a, struct value *b) {
  return pair(a, pair(b, NULL));
}

static void push(struct value **lastpair, struct value *element) {
  struct value *newpair;

  newpair = pair(element, NULL);
  (*lastpair)->v.pair.cdr = newpair;
  (*lastpair) = newpair;
}

static struct value *integers(const uintptr_t *elements, size_t count) {
  struct value *head;
  struct value *tail;

  head = tail = pair(NULL, NULL);
  for (; count; count--) {
    push(&tail, integer(*elements++));
  }
  return head->v.pair.cdr;
}

static struct value *symbols(const char **elements) {
  struct value *head;
  struct value *tail;

  head = tail = pair(NULL, NULL);
  for (; *elements; elements++) {
    push(&tail, symbol(*elements));
  }
  return head->v.pair.cdr;
}

static struct value *strings(const char **elements) {
  struct value *head;
  struct value *tail;

  head = tail = pair(NULL, NULL);
  for (; *elements; elements++) {
    push(&tail, string(*elements));
  }
  return head->v.pair.cdr;
}

// SRFI 176: Version flag

#if defined(__WATCOMC__)
const char *get_c_version(void) {
  static char buf[32];
  int major = __WATCOMC__ / 100;
  int minor = (__WATCOMC__ % 100) / 10;

  if (major > 11) {
    snprintf(buf, sizeof(buf), "Open Watcom %d.%d\n", major - 11, minor);
  } else {
    snprintf(buf, sizeof(buf), "Watcom %d.%d\n", major, minor);
  }
  return buf;
}
#elif defined(__GNUC__) // gcc and clang
const char *get_c_version(void) { return "GCC " __VERSION__; }
#else
const char *get_c_version(void) { return 0; }
#endif

#ifdef SCHEME_UNIX
static struct value *build_uname_list(void) {
  static struct utsname names;

  if (uname(&names) == -1) {
    return 0;
  }
  return pair(string(names.sysname),
              pair(string(names.release), pair(string(names.machine), 0)));
}
#else
static struct value *build_uname_list(void) { return 0; }
#endif

#ifdef SCHEME_UNIX
static const char *fdtype(int fd) {
  struct stat st;

  if (fstat(fd, &st) == 0) {
    if (S_ISFIFO(st.st_mode)) {
      return "pipe";
    }
    if (S_ISSOCK(st.st_mode)) {
      return "socket";
    }
    if (S_ISREG(st.st_mode)) {
      return "file";
    }
    if (S_ISCHR(st.st_mode) || S_ISBLK(st.st_mode)) {
      return isatty(fd) ? "terminal" : "device";
    }
  }
  return "unknown";
}
#else
static const char *fdtype(int fd) { return "unknown"; }
#endif

static struct value *build_stdio_list(void) {
  return pair(symbol(fdtype(STDIN_FILENO)),
              pair(symbol(fdtype(STDOUT_FILENO)),
                   pair(symbol(fdtype(STDERR_FILENO)), 0)));
}

static struct value *build_c_type_bits_list(void) {
  struct value *head;
  struct value *tail;

  head = tail = pair(NULL, NULL);
  push(&tail, list2(symbol("int"), integer(BITSIZEOF(int))));
  push(&tail, list2(symbol("long"), integer(BITSIZEOF(long))));
  push(&tail, list2(symbol("pointer"), integer(BITSIZEOF(void *))));
  push(&tail, list2(symbol("float"), integer(BITSIZEOF(float))));
  push(&tail, list2(symbol("double"), integer(BITSIZEOF(double))));
  push(&tail, list2(symbol("size_t"), integer(BITSIZEOF(size_t))));
  return head->v.pair.cdr;
}

static struct value *build_scheme_path_list(void) {
  static const char home[] = "/home/wiley/.local";
  struct value *head;
  struct value *tail;

  head = tail = pair(NULL, NULL);
  push(&tail, string(home));
  push(&tail, string("/usr/local/share/fantastic-scheme/1.x"));
  return head->v.pair.cdr;
}

static const char *lunar_phase(void) { return "waxing-crescent"; }

static struct value *build_version_alist(void) {
  static const char *languages[] = {"scheme", "r5rs", "r6rs", "r7rs", 0};
  static const char *encodings[] = {"utf-8", "iso-8859-1", 0};
  static const char *features[] = {"bits-64",
                                   "little-endian",
                                   "r7rs",
                                   "ratios",
                                   "exact-complex",
                                   "full-unicode",
                                   "little-endian",
                                   "fantastic-scheme",
                                   0};
  static const uintptr_t srfis[] = {0, 1, 2, 176};
  struct value *head;
  struct value *tail;

  head = tail = pair(NULL, NULL);

  push(&tail, list2(symbol("command"), string("fantastic-scheme")));
  push(&tail, list2(symbol("website"), string("https://example.com/scheme")));
  push(&tail, pair(symbol("languages"), symbols(languages)));
  push(&tail, pair(symbol("encodings"), symbols(encodings)));
  push(&tail, list2(symbol("install-dir"), string("/home/wiley/.local")));

  push(&tail, list2(symbol("scheme.id"), symbol("fantastic")));
  push(&tail, pair(symbol("scheme.srfi"),
                   integers(srfis, sizeof(srfis) / sizeof(srfis[0]))));
  push(&tail, pair(symbol("scheme.features"), symbols(features)));
  push(&tail, pair(symbol("scheme.path"), build_scheme_path_list()));

  push(&tail, list2(symbol("c.version"), string(get_c_version())));
  push(&tail, pair(symbol("c.compile"), strings(env_build_c_compile)));
  push(&tail, pair(symbol("c.link"), strings(env_build_c_link)));
  push(&tail, pair(symbol("c.type-bits"), build_c_type_bits_list()));

  push(&tail, list2(symbol("release"), string(env_release)));
  push(&tail, list2(symbol("release.date"), string(env_release_date)));
  push(&tail, list2(symbol("release.name"), string(env_release_name)));

  push(&tail, list2(symbol("build.date"), string(env_build_date)));
  push(&tail, list2(symbol("build.platform"), string(env_build_platform)));
  push(&tail, pair(symbol("build.configure"), strings(env_build_configure)));
  push(&tail, list2(symbol("build.git.tag"), string(env_build_git_tag)));
  push(&tail, list2(symbol("build.git.branch"), string(env_build_git_branch)));
  push(&tail, list2(symbol("build.git.commit"), string(env_build_git_commit)));
  push(&tail,
       pair(symbol("build.git.modified"), strings(env_build_git_modified)));

  push(&tail, pair(symbol("os.stdio"), build_stdio_list()));
  push(&tail, pair(symbol("os.uname"), build_uname_list()));

  push(&tail, list2(symbol("os.env.LANG"), string(getenv("LANG"))));
  push(&tail, list2(symbol("os.env.TERM"), string(getenv("TERM"))));

  push(&tail,
       list2(symbol("fantastic.phase-of-the-moon"), symbol(lunar_phase())));

  return head->v.pair.cdr;
}

// LOSE writer (Line-oriented S-Expressions), a bit paranoid

static int lose_is_valid(struct value *value) {
  if (value->type == T_PAIR) {
    for (; value; value = value->v.pair.cdr) {
      if (value->type != T_PAIR) {
        return 0;
      }
      if (!lose_is_valid(value->v.pair.car)) {
        return 0;
      }
    }
  } else if (value->type == T_SYMBOL) {
    const char *s = value->v.string;
    if (!isalpha(*s)) {
      return 0;
    }
    for (; *s; s++) {
      if (!isalnum(*s) && !strchr("_/*+.-", *s)) {
        return 0;
      }
    }
  } else if (value->type == T_STRING) {
    const char *s = value->v.string;
    for (; *s; s++) {
      if ((*s < 0x20) || (*s > 0x7e)) {
        return 0;
      }
    }
  } else if (value->type != T_INTEGER) {
    return 0;
  }
  return 1;
}

static void lose_write_nested(struct value *value) {
  if (value->type == T_PAIR) {
    putchar('(');
    for (;;) {
      lose_write_nested(value->v.pair.car);
      value = value->v.pair.cdr;
      if (value) {
        putchar(' ');
      } else {
        break;
      }
    }
    putchar(')');
  } else if (value->type == T_SYMBOL) {
    printf("%s", value->v.string);
  } else if (value->type == T_STRING) {
    const char *s = value->v.string;
    putchar('"');
    for (; *s; s++) {
      if ((*s == '\\') || (*s == '"')) {
        putchar('\\');
      }
      putchar(*s);
    }
    putchar('"');
  } else if (value->type == T_INTEGER) {
    printf("%lu", (unsigned long)value->v.integer);
  }
}

static void lose_write_valid_lines(struct value *list) {
  for (; list; list = list->v.pair.cdr) {
    if (lose_is_valid(list->v.pair.car)) {
      lose_write_nested(list->v.pair.car);
      putchar('\n');
    }
  }
}

// Main program

int main(int argc, char **argv) {
  int version;

  version = 0;
  if (argv[1] != NULL) {
    if (!strcmp(argv[1], "-V")) {
      version = 1;
    } else if (!strcmp(argv[1], "-:version")) {
      version = 1;
    }
  }
  if (version) {
    printf("Fantastic Scheme version %s"
           "\n"
           "Copyright (C) 2003 Pyrrhic Ventures"
           "\n"
           "This is free software; always read the label. There is NO warranty;"
           "\n"
           "not even for buoyancy or fitness for high-velocity landings at sea."
           "\n"
           "\n"
           "\"Shoot for the moon. Even if you miss, you'll crash on impact.\""
           "\n"
           "\n",
           env_release);
    lose_write_valid_lines(build_version_alist());
    exit(EXIT_SUCCESS);
  }
  fprintf(stderr, "Try -V or -:version\n");
  return EXIT_FAILURE;
}
