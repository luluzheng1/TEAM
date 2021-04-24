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

char* readline(FILE *fp)
{
  char *line_buf = NULL;
  size_t line_buf_size = 0;
  ssize_t line_size;
  assert(fp != NULL);
  line_size = getline(&line_buf, &line_buf_size, fp);

  return line_buf;
}

#ifdef BUILD_TEST
int main()
{
  FILE * fp = open("test.txt", "r");
  printf("%d", sizeof(fp));
  return 0;
}
#endif