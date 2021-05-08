// Tests lists with if and while statement 
list l = [];
l = append(l, 1);
if l[0] == 1:
    print("true\n");
else:
    print("false\n");
end

list l2 = [];
l2 = append(l2, 1);

int i = 0;
while(i < length(l2)):
    print("%d\n", l2[i]);
    i += 1;
end