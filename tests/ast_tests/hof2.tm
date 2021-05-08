int addTwoElements(int x, int y):
    return x + y;
end

list map2 ((int,int)->int function, list arr1, list arr2):
    list ans = [];
    int shorterArrLength = 0;

    if length(arr1) < length(arr2):
        shorterArrLength = length(arr1);
    else:
        shorterArrLength = length(arr2);
    end

    for i in 0..shorterArrLength:
        append(ans, function(arr1[i], arr2[i]));
    end
    
    return ans;
end

list arr1 = [1, 2, 3, 4];
list arr2 = [4, 3, 2, 1];
list arr = map2(addTwoElements, arr1, arr2);