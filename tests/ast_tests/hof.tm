int addOne(int x):
    return x + 1;
end

list map ((int)->int function, list arr):
    for i in 0..length(arr):
        arr[i] = function(arr[i]);
    end
    return arr;
end

list arr = [1, 2, 3, 4];
arr = map(addOne, arr);