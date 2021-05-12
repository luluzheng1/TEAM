bool contains(list l, list items):
    list res = [];
    for item in items:
        res = append(res, false);
        for ref in l:
            if ref == item:
                res[length(res) - 1] = true;
            end
        end
    end

    for r in res:
        if not r:
            return false;
        end
    end
    return true;
end

contains([1, 2], [1]);
contains(["hello", "wolrd"], ["hello"]);
contains(['h', 'e'], ['h']);
contains([true, false], [true]);
contains([1.1, 1.2], [1.1]);

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

list reverse(list a):
    list b = [];
    int len = length(a);
    for i in 0..len:
        b[0:0] = [a[i]];
    end
    return b;
end

reverse([1]);
reverse([[1]]);
reverse(["t"]);
reverse(['t']);
reverse([1.1]);
reverse([true]);

remove_int([1], 1, true);
remove_float([1.5], 1.5, false);
remove_bool([true], true, true);
remove_char(['a'], 'a', true);
remove_string(["hello"], "what", true);
