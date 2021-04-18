/*
list<string> test((int)->int hi, list<string> a):
  int i = 0;
  while i < 10:
    print("hi");
    i += 1;
  end

  return a;
end
*/


list<string> test2(list<string> a):
  int i = 0;
  while i < 10:
    print("hi");
    i += 1;
  end

  return a;
end


/*
float getAverage(list<int> nums):
  int sum = 0;
  for (int i = 0; i < len(nums); i++):
    sum += nums[i];
  end

  return sum / len(nums);
end

list<int> nums = [4, 5, 8, 1, 4, 10];
float avg = getAverage(nums);
print(avg);
*/