# Combo Breaker



This is my attempt to solve [Day 25](https://adventofcode.com/2020/day/25).


```r
sample <- read_lines("samples/day_25_sample.txt") %>% as.numeric()
actual <- read_lines("inputs/day_25_input.txt") %>% as.numeric()
```

## Part 1

We can brute force the loops by simply iterating until we find the card/door values we expect


```r
find_loops <- function(input) {
  card <- input[[1]]
  door <- input[[2]]
  
  value <- 1
  remainder <- 20201227
  
  i <- 1
  # initialise the loop values to a nonsense value so we can exit the loop once
  # we have found our values
  card_loop <- -1
  door_loop <- -1
  # we can calculate both values in a single loop to save repeated calculations
  while (card_loop < 0 || door_loop < 0) {
    value <- (value * 7) %% remainder
    if (card_loop == -1 && value == card) {
      card_loop <- i
    }
    
    if (door_loop == -1 && value == door) {
      door_loop <- i
    }
      
    i <- i + 1
  }
  
  c(card_loop, door_loop)
}
```

We can test that this works against the provided sample:


```r
sample_loops <- find_loops(sample)
all(sample_loops == c(8, 11))
```

```
## [1] TRUE
```

And now we can run this function to find the loops for the actual data:


```r
actual_loops <- find_loops(actual)
actual_loops
```

```
## [1]  8987376 14382089
```

Calculating the private key's is very simple, we just need to run the multiply / remainder logic as above `loop` times.


```r
find_key <- function(subject_number, loops) {
  value <- 1
  remainder <- 20201227
  
  for (i in 1:loops) {
    value <- (value * subject_number) %% remainder
  }
  
  value
}
```

We can test that this function works as expected against the sample data:


```r
find_key(sample[[1]], sample_loops[[2]]) == 14897079
```

```
## [1] TRUE
```

```r
find_key(sample[[2]], sample_loops[[1]]) == 14897079
```

```
## [1] TRUE
```

And we can run this function on the actual data:


```r
# the smaller loop size will be quicker to calculate
i <- which.min(actual_loops)
find_key(actual[[3 - i]], actual_loops[[i]])
```

```
## [1] 9620012
```

## Part 2

We are done for 2020! Hopefully this has been a useful resource for you!

---

*Elapsed Time: 6.339s*
