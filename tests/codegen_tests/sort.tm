list integers = [5,8,10,-6,25,3,-10];
integers[:length(integers)-1] = [2,6,2,43,7,8];
integers = reverse(integers);
int temp;
temp = integers[0];
for i in integers:
    print("%d\n", i);
end


