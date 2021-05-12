list integers = [5,8,10,-6,25,3,-10];

list sort(list unsorted):
    for i in 0..length(unsorted)-1:
        for j in i+1..length(unsorted):
            if unsorted[i] > unsorted[j]:
                int temp = unsorted[i];
                unsorted[i] = unsorted[j];
                unsorted[j] = temp;
            end
        end
    end
    return unsorted;
end

sort(integers);
for i in integers:
    print("%d\n", i);
end


