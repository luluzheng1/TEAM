int getAverage(list<int> nums, int length):
  int sum = 0;
  int i = 0;
  while (i < length):
    sum += nums[i];
    i += 1;
  end

  return sum / length;
end

list<int> duplicateIntList(list<int> intList):
  return intList;
end

list<int> nums = [10, 5, 8, 1, 4, 14];
int avg = getAverage(nums, 6);
print(avg);

list<int> result = duplicateIntList(nums);
result[0] = 4;
print(nums[0]);