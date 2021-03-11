/* Tests if, if-else, if-elsif-else 
   and single line if statements */

if x == 5:
    print(x);
else:
    print("not five");
end

if x == 5:
    print(x);
    if x == 4:
      print(x);
    end
elif x == 6:
    print(x);
else:
    print("not five");
end

if x == 5: print(x); else: print("not five"); end