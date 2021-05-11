// Reads in text from the file specified by input_file and replace
// all occurences of "built-in" to "standard" and output it to the file
// specified by output_file

string input_file = "./tests/test_text_files/TEAM_intro.txt";
string output_file = "./tests/test_text_files/TEAM_intro_edited.txt";

file input = open(input_file, "r");
list data = [];

string currline = readline(input);
while (not strcmp(currline, "")):
  data = append(data, currline);
  currline = readline(input);
end
close(input);

file output = open(output_file, "w");
for line in data:
  line = replaceall(line, "built-in", "standard");
  write(output, line);
end

close(output);