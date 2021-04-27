// Replace one
string s = replace("ggle google", "go*gle", "replaced", 1);
print(s);

// Replace more than all, caps at the 2 replace
s = replace("ggle google", "go*gle", "replaced", 5);
print(s);

// Replace nothing, returns original string
s = replace("ggle google", "go*gle", "replaced", 0);
print(s);

// Replace all
s = replace("google google google", "goo", "foo", 3);
print(s);