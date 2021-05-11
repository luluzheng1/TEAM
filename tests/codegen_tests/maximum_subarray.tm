/* Given an integer array nums, find the contiguous subarray (containing at least one number) which has the largest sum and return its sum. */
int max(int x, int y):
    if x > y:
        return x;
    end
    return y;
end

int maxSubArray(list nums):

    int total_sum = nums[0];
    int max_sum = nums[0];


    for num in nums[1:]:
        total_sum = max(total_sum + num, num);
        max_sum = max(max_sum, total_sum);
    end

    return max_sum;
end

print("%d\n", maxSubArray([-2,1,-3,4,-1,2,1,-5,4]));
// Output: 6
// Explanation: [4,-1,2,1] has the largest sum = 6.



