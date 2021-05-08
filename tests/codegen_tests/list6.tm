// Tests append on lists and nested list types

list d = append(["hello"], "world");
for i in d:
    print("%s", i);
end

list g = [[1]];
append(g, []);