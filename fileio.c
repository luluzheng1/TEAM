#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

FILE* open(char* filename, char* mode)
{
  FILE *fp;
  fp = fopen(filename, mode);
  return fp;
}

int close(FILE *fp)
{
  assert(fp != NULL);
  return fclose(fp);
}

char* readline(FILE *fp)
{
  assert(fp != NULL);
  char *line_buf = NULL;
  size_t line_buf_size = 0;
  getline(&line_buf, &line_buf_size, fp);

  return line_buf;
}

void write(FILE *fp, char* text)
{
  assert(fp != NULL);
  fputs(text, fp);
}

#ifdef BUILD_TEST
int main()
{
  return 0;
}
#endif