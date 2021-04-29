list<char> ASCII = [
                        'a', 'b', 'c', 'd', 'e',
                        'f', 'g', 'h', 'i', 'j',
                        'k', 'l', 'm', 'n', 'o',
                        'p', 'q', 'r', 's', 't', 
                        'u', 'v', 'w', 'x', 'y', 
                        'z', 
                        'A', 'B', 'C', 'D', 'E', 
                        'F', 'G', 'H', 'I', 'J', 
                        'K', 'L', 'M', 'N', 'O', 
                        'P', 'Q', 'R', 'S', 'T', 
                        'U', 'V', 'W', 'X', 'Y', 
                        'Z'
                    ];

/*
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

string join(list<string> text_list, string connector):
    string res = "";
    int list_length = length(text_list);
    for index in 0..(list_length - 1):
        res = (res + text_list[index] + connector);
    end 
    res = res + text_list[list_length - 1];
    return res;
end

string string_reverse(string text):
    string res = "";
    int string_length = length(text);
    int index = string_length - 1;
    while index >= 0:
        res = res + text[index];
        index -= 1;
    end 
    return res;
end

bool startWith(string text, char s):
    return s == text[0];
end

bool endWith(string text, char e):
    int string_length = length(text);
    return e == text[string_length - 1];
end







list<char> string_to_list(string text):
    list<char> result = [];
    for c in text:
        result = append(result, c);
    end
    return result;
end
*/

char lower(char c):
    int index = -1;
    for c_ref in ASCII:
        index += 1;
        if c == c_ref:
            if index < 26:
                return c;
            else:
                return ASCII[index - 26];
            end
        end
    end
    return c;
end

char upper(char c):
    int index = -1;
    for c_ref in ASCII:
        index += 1;
        if c == c_ref:
            if index > 25:
                return c;
            else:
                return ASCII[index + 26];
            end
        end
    end
    return c;
end

print("%c", lower('c'));