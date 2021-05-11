// Simple Test
list l = string_to_list("hello world");
for i in l:
    print("%c ", i);
end

// Loop Test
list colors = ["yellow", "white", "gray", "blue"];
for c in colors:
    list letters = string_to_list(c);

    for i in letters:
        print("%c ", i);
    end
end