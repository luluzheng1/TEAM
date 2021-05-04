list classes = ["HCI", "Programming Languages", "Compilers", "Security"];
for i in 0..4:
  print("%s\n", classes[i]);
end

print("\n");

list funClasses = classes[1:3];
for i in 0..2:
  print("%s\n", funClasses[i]);
end

print("\n");

list boringClasses = classes[:3];
for i in 0..3:
  print("%s\n", boringClasses[i]);
end

print("\n");

list moreClasses = append(classes, "Algo");
for i in 0..5:
  print("%s\n", moreClasses[i]);
end
