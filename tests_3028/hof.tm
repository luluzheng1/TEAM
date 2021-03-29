int addOne(int x)
    return x + 1;
end

list <int> map (int->int function, list <int> arr)
    for i in 0..length(arr):
        arr[i] = function(arr[i]);
    end
    return arr;
end

list <int> arr = [1, 2, 3, 4];
arr = map(addOne, arr);