// Tests returning empty list from a function
list bar(list l):
    l = append(l, "I am a string");
    return l;
end

list l = bar([]);
print("%d\n", length(l));