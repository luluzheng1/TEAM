bool contains_int(list l, int elem):
    for i in l:
        if i == elem:
            return true;
        end
    end

    return false;
end

bool contains_float(list l, float elem):
    for i in l:
        if i == elem:
            return true;
        end
    end

    return false;
end

bool contains_bool(list l, bool elem):
    for i in l:
        if i == elem:
            return true;
        end
    end

    return false;
end

bool contains_char(list l, char elem):
    for i in l:
        if i == elem:
            return true;
        end
    end

    return false;
end

bool contains_string(list l, string elem):
    for i in l:
        if strcmp(i, elem):
            return true;
        end
    end

    return false;
end


contains_int([1], 1);
contains_float([1.5], 1.5);
contains_bool([true], true);
contains_char(['a'], 'a');
contains_string(["hello"], "hello");