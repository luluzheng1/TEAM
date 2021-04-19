string s = find("hello hello hello", "hello");
print(s);

s = find("ggle google", "go*gle");
print(s);

s = find("goooogle", "go*gle");
print(s);

s = find("grey", "gr(a|e)y");
print(s);

s = find("There goes the bat", "[b-chm-pP]at|ot");
print(s);

s = find("a", "\\w");
print(s);

s = find("2+2", "\\d+[\\+-x\\*]\\d+");
print(s);

s = find("x+y", "\\w+[\\+-x\\*]\\w+");
print(s);

s = find("?+?", "\\W+[\\+-x\\*]\\W+");
print(s);

// Begins with dog
s = find("dog collar", "^dog");
print(s);

// No match
s = find("This is a dog", "^dog");
print(s);

// Ends with dog
s = find("hot dog", "dog$");
print(s);

// Just dog
s = find("dog", "^dog$");
print(s);

// Time in 24 hr format
s = find("12:03", "^([01]?[0-9]|2[0-3]):[0-5][0-9]$");
print(s);

// No match
s = find("z", "z{3,6}");
print(s);