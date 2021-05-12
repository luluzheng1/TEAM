// Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it is able to trap after raining.

// Input: [0,1,0,2,1,0,1,3,2,1,2,1]
// Output: 6

/* Two Pointer Approach
Keep two pointers, one starting at the beginning and one at the end of list. Use two variables, left_max and right_max to keep track of the highest bar seen so far. As long as current height of the left bar is less left_max or current height of the right bar is less than right_max, water is trapped. The water trapped depends on the left_max/right_max and the height of the current left/right bar. So, we can say that if there is a larger bar at one end (say right), we are assured that the water trapped would be dependant on height of bar in current direction (from left to right). As soon as we find the bar at other end (right) is smaller, we start iterating in opposite direction (from right to left). We will update left_max and right_max along the iteration. */


int trap(list height):
  int left = 0;
  int right = length(height) - 1;

  int left_max = 0;
  int right_max = 0;
  int water = 0;

  while left < right:
    if height[left] < height[right]:
      if height[left] >= left_max:
        left_max = height[left];
      else:
        water += left_max - height[left];
      end
      left += 1;
    else:
      if height[right] >= right_max:
        right_max = height[right];
      else:
        water += right_max - height[right];
      end
      right -= 1;
    end
  end

  return water;

end

print("%d\n", trap([0,1,0,2,1,0,1,3,2,1,2,1]));