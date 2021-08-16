#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char output[4096]; /* "\n" "real output" "\0" */

void init_from_stream(FILE *stream) {
  memset(output, 0, sizeof(output));
  output[0] = '\n';
  fread(&output[1], 1, sizeof(output) - 2, stream);
}

static char *skip_prefix(char *start, const char *key, int newline) {
  char prefix[32];
  char *newstart;

  snprintf(prefix, sizeof(prefix), "%s(%s ", (newline ? "\n" : ""), key);
  if ((newstart = strstr(start, prefix)) == NULL) {
    return NULL;
  }
  newstart += strlen(prefix);
  if (!newline) {
    for (; start < newstart; start++) {
      if ((*start == '\0') || (*start == '\n') || (*start == '\\')) {
        return NULL;
      }
    }
  }
  return newstart;
}

static char *take_until(char *start, int end_char) {
  char *limit;
  char *copy;

  for (limit = start; *limit != end_char; limit++) {
    if ((*limit == '\0') || (*limit == '\n') || (*limit == '\\')) {
      return "";
    }
  }
  if ((copy = calloc(1, limit - start + 1)) != NULL) {
    memcpy(copy, start, limit - start);
  }
  return copy;
}

const char *parse_string(const char *top) {
  char *start;

  if ((start = skip_prefix(output, top, 1)) == NULL) {
    return "";
  }
  if (*start != '"') {
    return "";
  }
  return take_until(start + 1, '"');
}

const char *parse_symbol(const char *top) {
  char *start;

  if ((start = skip_prefix(output, top, 1)) == NULL) {
    return "";
  }
  return take_until(start, ')');
}

const char *parse_subsymbol(const char *top, const char *sub) {
  char *start;

  if ((start = skip_prefix(output, top, 1)) == NULL) {
    return "";
  }
  if ((start = skip_prefix(start, sub, 0)) == NULL) {
    return "";
  }
  return take_until(start, ')');
}
