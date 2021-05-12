/* This program maximizes profit by choosing a single day to buy 
 * one stock and choosing a different day in the future to sell
 * that stock.
 * Return the maximum profit one can achieve from this transaction. 
 * If no profits can be achieved, return 0. */

int max_profit(list stock_prices):
    int min_price = stock_prices[0];
    int max_profit = 0;

    for i in 1..length(stock_prices)-1:
        int current_price = stock_prices[i];
        int next_price = stock_prices[i+1];
        if current_price > next_price:
            int potential_profit = current_price - min_price;
            max_profit += potential_profit;
            min_price = stock_prices[i+1];
        else:
            if min_price > current_price:
                min_price = current_price;
            end
        end
    end
    return max_profit;
end

print("%d\n", max_profit([5,2,6,9,3]));