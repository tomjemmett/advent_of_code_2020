# Rambunctious Recitation



This is my attempt to solve [Day 15](https://adventofcode.com/2020/day/15).


```r
sample <- c(0,3,6)
actual <- c(12,1,16,3,11,0)
```

## Part 1

Ideally we would use a data type like a dictionary in Python for today's problem... but there isn't really a good
option in R. Instead I will just assign a vector that will be long enough for the solution. I'm going to make an
assumption that the largest number we will see will be less than the number of turns.


```r
solve <- function(input, turns) {
  turn <- numeric(turns)
  
  # first iterate over the input
  for (i in seq_along(input)) {
    p <- input[[i]]
    # R is 1 indexed, make it 0
    turn[[p + 1]] <- i
  }
  
  # now continue for the rest of the turns
  for (i in (length(input) + 1):turns) {
    # find out which value was spoke
    t <- turn[[p + 1]]
    # update the value of the turn
    turn[[p + 1]] <- i - 1
    # find which word is to be spoke
    p <- ifelse(t == 0, 0, i - t - 1)
  } 
  
  p
}
```

We can now test that our function works against the sample data:


```r
solve(sample,  4) == "0"
```

```
## [1] TRUE
```

```r
solve(sample,  5) == "3"
```

```
## [1] TRUE
```

```r
solve(sample,  6) == "3"
```

```
## [1] TRUE
```

```r
solve(sample,  7) == "1"
```

```
## [1] TRUE
```

```r
solve(sample,  8) == "0"
```

```
## [1] TRUE
```

```r
solve(sample,  9) == "4"
```

```
## [1] TRUE
```

```r
solve(sample, 10) == "0"
```

```
## [1] TRUE
```

```r
solve(sample, 2020) == 436
```

```
## [1] TRUE
```

We can now run our function on the actual data:


```r
solve(actual, 2020)
```

```
## [1] 1696
```

## Part 2

We don't need to change anything for part 2, other than the number of turns.


```r
solve(actual, 30000000)
```

```
## [1] 37385
```

---

*Elapsed Time: 1.247s*
