/* Tests if, if-else, if-elsif-else 
   and single line if statements */
int x = 5;

if x == 5:
    print("%d\n", x);
else:
    print("not five");
end

if x == 5:
    print("%d\n", x);
    if x == 4:
      print("%d\n", x);
    end
elif x == 6:
    print("%d\n", x);
else:
    print("not five");
end

if x == 5: print("%d\n", x); else: print("not five"); end