int change_possibilities(int amount_left, list denominations, int current_index):
    if amount_left == 0:
        return 1;
    elif amount_left < 0:
        return 0;
    elif current_index == length(denominations):
        return 0;
    end

    int current_coin = denominations[current_index];

    int num_possibilities = 0;
    while amount_left >= 0:
        num_possibilities += change_possibilities(amount_left, denominations, current_index + 1);
        amount_left -= current_coin;
    end
    return num_possibilities;
end

print("Expected: 4, Got: %d\n", change_possibilities(4, [1,2,3], 0));

print("Expected: 1, Got: %d\n", change_possibilities(0, [1,2], 0));

print("Expected: 0, Got: %d\n", change_possibilities(5, [25, 50], 0));

print("Expected: 6, Got: %d\n", change_possibilities(50, [5, 10], 0));

print("Expected: 1, Got: %d\n", change_possibilities(1, [], 0));



