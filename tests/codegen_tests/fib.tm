// Compute the nth Fibonacci number

int fib(int n):

    if n == 0 or n == 1:
        return n;
    end
    int prev_prev = 0;
    int prev = 1;

    int current;
    for i in 0..n-1:
        current = prev_prev + prev;
        prev_prev = prev;
        prev = current;
    end
    return current;
end

print("The third fibonacci number is %d\n", fib(3));