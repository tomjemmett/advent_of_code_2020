# Shuttle Search



This is my attempt to solve [Day 13](https://adventofcode.com/2021/day/13).


```r
sample <- read_lines("samples/day_13_sample.txt")
actual <- read_lines("inputs/day_13_input.txt")
```

## Part 1

We can solve part 1 by finding the first time each of the buses arrives after we arrive, then finding which bus has the
minimum time.


```r
part_1 <- function(input) {
  earliest_time <- as.integer(input[[1]])
  buses <- input[[2]] %>%
    str_split(",") %>%
    pluck(1) %>%
    str_subset("\\d") %>%
    as.integer()
  
  first_available <- ceiling(earliest_time / buses) * buses
  which_bus <- which.min(first_available)
  (first_available[[which_bus]] - earliest_time) * buses[[which_bus]]
}
```

We can check that this function works on the sample data:


```r
part_1(sample) == 295
```

```
## [1] TRUE
```

And we can run with the actual data


```r
part_1(actual)
```

```
## [1] 3269
```

## Part 2

Part 2 could be solved with the [Chinese Remainder Theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem),
but we could solve it more simply by iterating over the input items and keeping track of the current time and the
current "jump" size. The jump size is equal to the cumulative product of the busses.


```r
part_2 <- function(input) {
  # take the 2nd row of the input, convert x's to 0's
  x <- input[[2]] %>%
    str_replace_all("x", "0") %>%
    str_split(",") %>%
    pluck(1) %>%
    as.numeric()
  
  # get the sequence of values that are non zero, index from 0
  y <- seq_along(x)[x != 0] - 1
  # get rid of the 0's (x's)
  x <- x[x != 0]
  
  # start with a jump equal to the first value in the input
  j <- x[1]
  # start at time 0
  t <- 0
  
  # iterate over the input, skipping the first item
  for (i in 2:length(x)) {
    # increase our time by the size of the jump while the time + step size is
    # not a divisor of the current input item
    while ((t + y[[i]]) %% x[[i]]) {
      t <- t + j
    }
    # increase the size of our jump by the current input item (bus)
    j <- j * x[[i]]
  }
  # return the time
  t
}
```

We can run this function on the sample data.


```r
part_2(sample) == 1068781
```

```
## [1] TRUE
```

And we can run on the actual data. We need to make sure that we alter the "scientific notation penalty" option to view
the answer as an integer.


```r
options(scipen = 999)
part_2(actual)
```

```
## [1] 672754131923874
```
