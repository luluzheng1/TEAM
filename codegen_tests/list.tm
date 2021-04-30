// Tests polymorphic function and returning list from function
list foo(list l):
    print("%d\n", length(l));
    return l;
end

list mylist = foo([1,2,3]);

list anotherlist = foo([1.5]);