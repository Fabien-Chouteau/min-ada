#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "min.h"

struct min_context test_ctx;

struct min_context*
get_ctx (void)
{
  return &test_ctx;
}

void
min_debug_print (const char *format, ...)
{
  va_list ap;
  const char *prefix = "C DBG: ";
  char *newformat = malloc (strlen (prefix) + strlen (format) + 1);

  strcpy (newformat, prefix);
  strcat (newformat, format);

  va_start(ap, format);
  vprintf (newformat, ap);
	va_end(ap);

  free (newformat);
}
