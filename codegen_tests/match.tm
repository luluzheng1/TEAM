bool b = match("gray", "gr(a|e)y");
print(b);

// Begins with dog
b = match("dog collar", "^dog");
print(b);

// Ends with dog
b = match("hot dog", "dog$");
print(b);

// Just dog
b = match("dog", "^dog$");
print(b);

// No match
b = match("This is a dog", "^dog");
print(b);

// Time in 24 hr format
b = match("12:03", "^([01]?[0-9]|2[0-3]):[0-5][0-9]$");
print(b);

b = match("2+2", "\\d+[\\+-x\\*]\\d+");
print(b);
b = match("x+y", "\\w+[\\+-x\\*]\\w+");
print(b);
b = match("?+?", "\\W+[\\+-x\\*]\\W+");
print(b);
