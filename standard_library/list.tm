bool contains(list l, list items):
    list res = [];
    for item in items:
        res = append(res, False);
        for ref in l:
            if ref == item:
                res[length(res)] = true;
            end
        end

    end

    for r in res:
        if not r:
            return False;
        end
    end
    return True;
end

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

list remove_int(list l, int elem, bool all):
    list retlist = [];
    int i = 0;
    int len = length(l);
    int remove_index = 0;

    if all:
        while i < len:
            if l[i] == elem:
                i += 1;
                continue;
            end
            retlist = append(retlist, l[i]);
            i += 1;
        end   
    else:
        while i < len:
            if l[i] == elem:
                remove_index = i;
                break;
            end 
            
            retlist = append(retlist, l[i]);
            
            i += 1;
        end
        /* can be optimized with list concat */
        if i != len:
            for index in remove_index+1..len:
                retlist = append(retlist, l[index]);
            end
        end
    end

    return retlist; 
end


list remove_float(list l, float elem, bool all):
    list retlist = [];
    int i = 0;
    int len = length(l);
    int remove_index = 0;

    if all:
        while i < len:
            if l[i] == elem:
                i += 1;
                continue;
            end
            retlist = append(retlist, l[i]);
            i += 1;
        end   
    else:
        while i < len:
            if l[i] == elem:
                remove_index = i;
                break;
            end 
            
            retlist = append(retlist, l[i]);
            
            i += 1;
        end
        /* can be optimized with list concat */
        if i != len:
            for index in remove_index+1..len:
                retlist = append(retlist, l[index]);
            end
        end
    end

    return retlist; 
end

list remove_bool(list l, bool elem, bool all):
    list retlist = [];
    int i = 0;
    int len = length(l);
    int remove_index = 0;

    if all:
        while i < len:
            if l[i] == elem:
                i += 1;
                continue;
            end
            retlist = append(retlist, l[i]);
            i += 1;
        end   
    else:
        while i < len:
            if l[i] == elem:
                remove_index = i;
                break;
            end 
            
            retlist = append(retlist, l[i]);
            
            i += 1;
        end
        /* can be optimized with list concat */
        if i != len:
            for index in remove_index+1..len:
                retlist = append(retlist, l[index]);
            end
        end
    end

    return retlist; 
end


list remove_char(list l, char elem, bool all):
    list retlist = [];
    int i = 0;
    int len = length(l);
    int remove_index = 0;

    if all:
        while i < len:
            if l[i] == elem:
                i += 1;
                continue;
            end
            retlist = append(retlist, l[i]);
            i += 1;
        end   
    else:
        while i < len:
            if l[i] == elem:
                remove_index = i;
                break;
            end 
            
            retlist = append(retlist, l[i]);
            
            i += 1;
        end
        /* can be optimized with list concat */
        if i != len:
            for index in remove_index+1..len:
                retlist = append(retlist, l[index]);
            end
        end
    end

    return retlist; 
end

list remove_string(list l, string elem, bool all):
    list retlist = [];
    int i = 0;
    int len = length(l);
    int remove_index = 0;

    if all:
        while i < len:
            if strcmp(l[i], elem):
                i += 1;
                continue;
            end
            retlist = append(retlist, l[i]);
            i += 1;
        end   
    else:
        while i < len:
            if strcmp(l[i], elem):
                remove_index = i;
                break;
            end 
            
            retlist = append(retlist, l[i]);
            
            i += 1;
        end
        /* can be optimized with list concat */
        if i != len:
            for index in remove_index+1..len:
                retlist = append(retlist, l[index]);
            end
        end
    end

    return retlist; 
end

remove_int([1], 1, true);
remove_float([1.5], 1.5, false);
remove_bool([true], true, true);
remove_char(['a'], 'a', true);
remove_string(["hello"], "what", true);
