/* Tests functions, formal parameters, 
   and statements within functions */

int add(int x, int y):
  int z = x + y; 
  return z;
end

int subtract(int x, int y):
  int z = x - y; 
  return z;
end

int a = add(3, 4); int b = subtract(10,99);
int c = a + b;