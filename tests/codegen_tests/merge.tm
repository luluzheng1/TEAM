// Merge function for merge sort

list merge(list left, list right):
    if length(left) == 0:
        return right;
    end

    if length(right) == 0:
        return left;
    end

    list result = [1];
    result = [];
    int index_left = 0;
    int index_right = 0;

    while length(result) < length(left) + length(right):
        if left[index_left] <= right[index_right]:
            result = append(result, left[index_left]);
            index_left += 1;
        else:
            result = append(result, right[index_right]);
            index_right += 1;
        end
        if index_right == length(right):
            result = result + left[index_left:];
            break;
        end
        if index_left == length(left):
            result = result + right[index_right:];
            break;
        end
    end
    return result;
end

merge([1,2,3], [-1,-2,-3]);