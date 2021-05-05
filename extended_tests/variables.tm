int num = 40;
print("%d\n", num);

void test(int num):
  print("%d\n", num);
end

test(4);

void test2():
  int num = 20;
  print("%d\n", num);
end

test2();
print("%d\n", num);

void test3():
  num = 60;
end

test3();
print("%d\n", num);