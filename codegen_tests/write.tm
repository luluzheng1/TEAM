list roster = ["Lulu", "Yingjie", "Saurav", "Naoki"];

file output = open("./test_text_files/output1.txt", "w");

for name in roster:
  write(output, name + "\n");
end

close(output);