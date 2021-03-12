/* Tests faulty functions; missing parens
   for formal params */

void add int x, int y
  print(x);
  int z = x + y;
  print(z);
  return;
end