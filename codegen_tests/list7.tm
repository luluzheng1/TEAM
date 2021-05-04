// Tests passing list into multi-argument functions and returning a list from the function

list foo(list l, string s):
    for i in l:
        print("%d\n", i);
    end
    return l;
end

list f = foo([1,2], "hello");
