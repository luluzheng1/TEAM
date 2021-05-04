// Tests continue while loop
int i = 0;
while i < 10:
  i = i + 2;
  if (i == 6):
    continue;
  end
  print("%d\n",i);
end