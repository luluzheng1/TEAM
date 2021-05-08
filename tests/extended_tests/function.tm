int getAverage(list nums, int length):
  int sum = 0;
  int i = 0;
  while (i < length):
    sum += nums[i];
    i += 1;
  end

  return sum / length;
end

list duplicateIntList(list intList):
  return intList;
end

list nums = [10, 5, 8, 1, 4, 14];
int avg = getAverage(nums, 6);
print("%d\n", avg);

list result = duplicateIntList(nums);
result[0] = 4;
print("%d\n", nums[0]);