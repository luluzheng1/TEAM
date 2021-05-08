// Test passing multiple lists into a function

list foo(list l, list l2):
    for i in l:
        print("%d\n", i);
    end
    for i in l2:
        print("%s\n", i);
    end   
    return l;
end

list mylist = foo([1,2,3], ["one", "two"]);
print("%d\n", length(mylist));