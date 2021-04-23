list<unknown> reverse(list<unknown> a):
    list<unknown> b = [];
    for index in 0..length(a):
        append(b, a[index], 0);
    end
    return b;
end