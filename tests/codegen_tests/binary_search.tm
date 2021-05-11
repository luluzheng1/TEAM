bool binary_search(int target, list nums):
    int floor_index = -1;
    int ceiling_index = length(nums);
    while floor_index + 1 < ceiling_index:
        int distance = ceiling_index - floor_index;
        int half_distance = distance / 2;
        int guess_index = floor_index + half_distance;

        if nums[guess_index] == target:
            return true;
        elif nums[guess_index] > target:
            ceiling_index = guess_index;
        else:
            floor_index = guess_index;
        end
    end

    return false;

end

list l = [1,3,7,10,15,20];
print("%s\n", binary_search(1, l));
print("%s\n", binary_search(14, l));