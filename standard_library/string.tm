list split(string text, char separator):
    list result = [];
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

list string_to_list(string text):
    list result = [];
    for c in text:
        result = append(result, c);
    end
    return result;
end

list result = split("hello,tim", ',');
print("%d\n", length(result));
print("%s\n", result[0]);
print("%s\n", result[1]);