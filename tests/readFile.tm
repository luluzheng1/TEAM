// Tests read file TODO needs file keyword before filereader

filereader = open("file.txt");
list <string> lines = [];

while filereader == true:
  string currline = readline(filereader);
  append(lines, currline);
end
