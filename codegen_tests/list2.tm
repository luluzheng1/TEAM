// Tests list slicing and list accessing

list a = [1,2,3];
a[0];

list b = [];
b = [1, 2, 3];
print_list(b[0:2]);
b[0:2] = [3];
print_list(b[0:2]);
list right_slice = b[1:];
print_list(right_slice);
list c = [1, 2, 3];
a = c[0:1];
print_list(a);

a = c[:1];
print_list(a);
void print_list(list l):
    print("[");
    for i in l:
        print("%d", i);
        if i != l[length(l)-1]:
            print(",");
        end
    end
    print("]\n");
end