## Ex 1. What is the result of these two lines?

```erl
{A, B} = {2, 3}.
B.
```

### Answer.

```
1> {2,3} % This lines matches A with 2 and B with 3. They are permanently "assigned" (there is no reassignment)
2> 3 % Line two outputs the value of B which is 3 as per the last line
```

## Ex 2. What is the result of these two lines, if they’re typed after the previous two?

```erl
{A,C} = {2,5}.
{A,D} = {6,6}.
```

### Answer.

```
{2, 5} % This line matches A with two (which it already is) and C with 5.
** exception error: no match of right hand side value {6,6}. % This line errors because A is 2, not 6.
```

## Exercise 3. What is the output of each of these lines?

```erl
A = 2+3.
B=A-1.
A=B+1.
A=B.
```

### Answer.

```
1> 5
2> 4
3> 5
4> ** exception error: no match of right hand side value 4
```

## Exercise 4. What is the output of each of these lines?

```erl
f(A).
A=B.
f().
```

### Answer.

```
1> ok
2> 4
3> ok
```
