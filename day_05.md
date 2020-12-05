# Binary Boarding



This is my attempt to solve [Day 5](https://adventofcode.com/2020/day/5).


```r
sample <- read_lines("samples/day_05_sample.txt")
actual <- read_lines("inputs/day_05_input.txt")
```

## Part 1

Today's challenge can be solved with a recursive function where at each step we divide the search space in half. The
same algorithm (`partition()`) will work for both the rows and the columns, we just need to adjust the starting point
(`i`) and the `min` and `max` values. Because we are dividing the search space in half at each iteration when the `min`
and `max` values are equal we can simply return either value.

The `s` argument to `partition()` should be a vector of individual characters, e.g. `c("a", "b", "c")`.


```r
partition <- function(s, i, min, max) {
  if (min == max) {
    return (min)
  }
  
  # what is the distance between min and max?
  d <- max - min + 1
  # find the half way point
  m <- d %/% 2
  
  # update either the min or max position
  if (s[[i]] == "F" | s[[i]] == "L") {
    max <- max - m
  } else {
    min <- min + m
  }
  
  # call partition on the next character
  partition(s, i + 1, min, max)
}
```

We can now build a function to process each string and return the data as a list.


```r
process_string <- function(string) {
  s <- str_extract_all(string, ".")[[1]]
  
  row <- partition(s, 1, 0, 127)
  col <- partition(s, 8, 0, 7)
  
  list(
    row = row,
    col = col,
    seat = row * 8 + col
  )
}
```

And now we can our function on the sample data.


```r
sample %>%
  map_dfr(process_string) %>%
  mutate(str = sample, .before = everything())
```

```
## # A tibble: 3 x 4
##   str          row   col  seat
##   <chr>      <dbl> <dbl> <dbl>
## 1 BFFFBBFRRR    70     7   567
## 2 FFFBBBFRRR    14     7   119
## 3 BBFFBBFRLL   102     4   820
```

This matches the provided example, so we can run the function on our actual data.


```r
actual %>%
  map_dfr(process_string) %>%
  pull(seat) %>%
  max()
```

```
## [1] 816
```

## Part 2

For part 2 we can use the same function from part 1 to get the list of seats. Then, we can find the minimum and maximum
seat number and create the range of values, then simply find the seat that isn't in this range.


```r
seats <- actual %>%
  map_dfr(process_string) %>%
  pull(seat)

range <- min(seats):max(seats)
range[!range %in% seats]
```

```
## [1] 539
```

## Extra: Solving algebraicly

Triangle numbers are defined as:

$$
\sum_{n=1}^{N} n = \frac{n(n + 1)}{2}
$$

If we were to find the triangle number of the maximum value and subtract the triangle number of one less than the
smallest seat that would give us the sum of the seats if all were occupied. So if we simply subtract the sum of the
seats this will leave us with the empty seat.


```r
triangle_number <- function(n) 0.5 * n * (n + 1)

triangle_number(max(seats)) - triangle_number(min(seats) - 1) - sum(seats)
```

```
## [1] 539
```
