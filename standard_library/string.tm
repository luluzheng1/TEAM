list<string> split(string text, char separator):
    list<string> result = [];
    int text_length = length(text);
    int left = 0;
    int right = 0;
    while right < text_length:
        if text[right] == separator:
            result = append(result, text[left:right]);
            right = right + 1;
            left = right;
        else:
            right = right + 1;
        end
    end
    result = append(result, text[left:right]);
    return result;
end 

list<char> string_to_list(string text):
    list<char> result = [];
    for c in text:
        result = append(result, c);
    end
    return result;
end

list<string> result = split("hello,tim", ',');
print(length(result));
print(result[0]);
print(result[1]);