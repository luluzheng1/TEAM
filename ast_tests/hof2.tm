int addTwoElements(int x, int y):
    return x + y;
end

list <int> map2 ((int,int)->int function, list <int> arr1, list <int> arr2):
    list <int> ans = [];
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

list <int> arr1 = [1, 2, 3, 4];
list <int> arr2 = [4, 3, 2, 1];
arr = map2(addTwoElements, arr1, arr2);