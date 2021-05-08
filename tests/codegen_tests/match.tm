bool b = match("gray", "gr(a|e)y");
print("%s\n", b);

// Begins with dog
b = match("dog collar", "^dog");
print("%s\n", b);

// Ends with dog
b = match("hot dog", "dog$");
print("%s\n", b);

// Just dog
b = match("dog", "^dog$");
print("%s\n", b);

// No match
b = match("This is a dog", "^dog");
print("%s\n", b);

// Time in 24 hr format
b = match("12:03", "^([01]?[0-9]|2[0-3]):[0-5][0-9]$");
print("%s\n", b);

b = match("2+2", "\\d+[\\+-x\\*]\\d+");
print("%s\n", b);
b = match("x+y", "\\w+[\\+-x\\*]\\w+");
print("%s\n", b);
b = match("?+?", "\\W+[\\+-x\\*]\\W+");
print("%s\n", b);
