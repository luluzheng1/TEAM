// Tests polymorphic function and returning list from function
list foo(list l):
    print("%d\n", length(l));
    return l;
end

list intlist = foo([1,2,3]);

list floatlist = foo([1.5]);

list charlist = foo(['a', 'b', 'c', 'd']);

list stringlist = foo(["hello", "world", "goodbye", "world"]);

list listoflist = foo([["hello"], ["world"], ["goodbye"], ["world"]]);