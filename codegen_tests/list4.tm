// Tests returning empty list from a function
list bar(list l):
    return l;
end

list l = bar([]);
print("%d\n", length(l));