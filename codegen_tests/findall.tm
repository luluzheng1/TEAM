// Three matches
list<string> l = findall("hello hello hello", "hello");
string s = l[0];
string s2 = l[1];
string s3 = l[2];
print("%s\n", s);
print("%s\n", s2);
print("%s\n", s3);

// Three matches
l = findall("google ggle goooogle", "go*gle");
s = l[0];
s2 = l[1];
s3 = l[2];
print("%s\n", s);
print("%s\n", s2);
print("%s\n", s3);

// One match
l = findall("gray", "gr(a|e)y");
s = l[0];
print("%s\n", s);

// Five matches
l = findall("2+2 3*3 4-4 5+5 6*6", "\\d+[\\+-x\\*]\\d+");
s = l[0];
s2 = l[1];
s3 = l[2];
string s4 = l[3];
string s5 = l[4];
print("%s\n", s);
print("%s\n", s2);
print("%s\n", s3);
print("%s\n", s4);
print("%s\n", s5);

// Zero matches
l = findall("This is a dog", "^dog");