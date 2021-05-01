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
