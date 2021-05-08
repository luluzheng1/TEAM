file input = open("./tests/test_text_files/input1.txt", "r");
string currline;

while (not strcmp(currline = readline(input), "")):
  print(currline);
end

close(input);