// Find a number that appears more than once
int find_repeat(list nums):
    int floor = 1;
    int ceiling = length(nums) - 1;

    while floor < ceiling:
        int half_distance = (ceiling - floor) / 2;
        int midpoint = floor + half_distance;

        int lower_floor = floor;
        int lower_ceiling = midpoint;
        int upper_floor = midpoint + 1;
        int upper_ceiling = ceiling;

        int items_in_lower_range = 0;
        for i in nums:
            if i >= lower_floor and i <= lower_ceiling:
                items_in_lower_range += 1;
            end
        end

        int number_of_distinct_integers = lower_ceiling - lower_floor + 1;
        if items_in_lower_range > number_of_distinct_integers:
            floor = lower_floor;
            ceiling = lower_ceiling;
        else:
            floor = upper_floor;
            ceiling = upper_ceiling;
        end
    end

    return floor;
end

int repeated = find_repeat([1,2,3,4,4,5]);
print("%d\n", repeated);