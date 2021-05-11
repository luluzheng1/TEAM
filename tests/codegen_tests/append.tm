list data = [];

int i = 10;
while i > 0:
  data = append(data, "hello");
  i -= 1;
end

for d in data:
  print("%s ", data[i]);
end 