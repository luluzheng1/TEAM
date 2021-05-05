// Tests the replaceAll function in regex built-in library

// Replace all with "replaced"
string s = replaceall("google ggle goooogle", "go*gle", "replaced");
print("%s\n", s);

// Replace grey and gray with "brown"
s = replaceall("This dog is grey and this cat is gray.", "gr(a|e)y", "brown");
print("%s\n", s);

// Replace hello with "bye
s = replaceall("Hello hello hello", "hello", "bye");
print("%s\n", s);