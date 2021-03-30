// Tests binary operators

// int + int = int
5 + 5;
3 - 5;
5 * 5;
5 / 5;

// float + float = float
5.1 + 10.5;
10.5 - 5.1;
5.1 * 10.5;
10.5 / 5.1;

// int + float = float
10 + 5.1;


// float + int = float
5.1 + 10;

// int ^ int = int
5^2;

// float ^ float = float
5.1^10.5;

// int ^ float = float
5^2.1;

// float ^ int = float
2.1^5;

// expects bool type
"Hello" == "Hello";
5 == 5;
5.1 != 6.1;

5 < 6;
6.1 > 7.1;
5 <= 8;
7.1 >= 8.1;

5 < 6 and 6.1 > 7.1;
5 <= 8 and 7.1 >= 8.1;

// expects list int
5..6;