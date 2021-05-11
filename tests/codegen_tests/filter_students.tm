// Reads in a list of students from the file specified by input_file and output
// names of students who are in the class of 2021 and studying CS to the file
// specified by output_file

string input_file = "./tests/test_text_files/roster.txt";
string output_file = "./tests/test_text_files/cs2021.txt";

list data = [];
file input = open(input_file, "r");

string currline = readline(input);
while (not strcmp(currline, "")):
  data = append(data, currline);
  currline = readline(input);
end
close(input);

file output = open(output_file, "w");
for line in data:
  list data = split(line, ' ');
  
  if data[1] == "2021" and data[2] == "CS\n":
    write(output, data[0] + "\n");
  end
end

close(output);