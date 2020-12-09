# Encoding Error



This is my attempt to solve [Day 9](https://adventofcode.com/2020/day/9).


```r
sample <- read_lines("samples/day_09_sample.txt") %>% as.numeric()
actual <- read_lines("inputs/day_09_input.txt") %>% as.numeric()
```

## Part 1

We can solve part 1 by creating a function that slides a window (`r`) across the `input` and checks to see if the value
of the element `i` just after the end of the current window, minus all of the values in that window, is in the window.
  

```r
part_1 <- function(input, n) {
  i <- n + 1
  r <- 1:n
  while (any((input[[i]] - input[r]) %in% input[r])) {
    r <- r + 1
    i <- i + 1
  }
  input[[i]]
}
```

We can now check our function gives us the right answer for the sample:


```r
ps1 <- part_1(sample, 5)
ps1 == 127
```

```
## [1] TRUE
```

This gives us the correct answer, so we can now run on the actual input:


```r
pa1 <- part_1(actual, 25)
pa1
```

```
## [1] 57195069
```

## Part 2

The naive approach here is to start to use nested for loops to sum the values. There is probably a smarter way of
solving this, but it's the approach I will use.

Instead of using the `sum`, `min` and `max` functions available in R I am going to keep track of the values: this should
give us a slight performance boost by not having to iterate through all of the values each time.


```r
part_2 <- function(input, t) {
  s <- which(input == t)
  # we need to loop until just before the index of the target in the input
  # as we are using nested loops it should be s - 2 here
  for (i in 1:(s - 2)) {
    sv <- input[[i]] # keep track of the sum
    minv <- sv       # and the min value
    maxv <- sv       # and the max value
    # now we need to loop from the next value (i + 1) to just before the target
    for (j in (i + 1):(s - 1)) {
      # update the sum
      sv <- sv + input[[j]]
      # update the min and max values
      if (input[[j]] < minv) minv <- input[[j]]
      if (input[[j]] > maxv) maxv <- input[[j]]
      # if our sum is equal to the target, then return the sum of the min and max value
      if (sv == t) {
        return (minv + maxv)
      }
    }
  }
}
```

We can test on the sample:


```r
part_2(sample, ps1) == 62
```

```
## [1] TRUE
```

And run on the actual input:


```r
part_2(actual, pa1)
```

```
## [1] 7409241
```
