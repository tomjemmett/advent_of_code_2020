# Report Repair



This is my attempt to solve [Day 1](https://adventofcode.com/2020/day/1).


```r
sample <- read_lines("samples/day_01_sample.txt") %>% as.integer()
actual <- read_lines("inputs/day_01_input.txt") %>% as.integer()
```

## Part 1

The naive approach to solving today's problem is to simple loop through the list twice, checking to see if the condition
is met. If it is, immediately return that value. If we reach the end of both loops without finding the solution we can
return NULL to indicate no result found.


```r
part_1_naive <- function(input, target) {
  if (length(input) == 0) stop("empty list")
  
  for (i in 1:(length(input)-1)) {
    for (j in (i+1):length(input)) {
      if (input[[i]] + input[[j]] == target)
        return (input[[i]] * input[[j]])
    }
  }
  NULL
}
```

We can test that this method works:

```r
part_1_naive(sample, 2020) == 514579
```

```
## [1] TRUE
```

This approach could be improved, we can sort the list, then create two pointers: the start and end of the list. If we
add these two numbers up and exceed the target then we can decrease the higher number pointer. If the number is lower
that the target we increase the lower number pointer.


```r
part_1_improved <- function(input, target) {
  if (length(input) == 0) stop("empty list")
  
  i <- 1
  j <- length(input)
  
  input <- sort(input)
  
  while (i <= j) {
    v <- input[[i]] + input[[j]]
    if (v == target) {
      return (input[[i]] * input[[j]])
    } else if (v < target) {
      i <- i + 1
    } else {
      j <- j - 1
    }
  }
  
  NULL
}
```

Again, we can test that this method works.


```r
part_1_improved(sample, 2020) == 514579
```

```
## [1] TRUE
```

We can now use our improved algorithm to get the result for part 1:


```r
part_1_improved(actual, 2020)
```

```
## [1] 793524
```

### is the improved algorithm any better?


```r
bench::mark(part_1_naive(sample, 2020),
            part_1_improved(sample, 2020))
```

```
## # A tibble: 2 x 6
##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
##   <bch:expr>                    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
## 1 part_1_naive(sample, 2020)      2.76µs    3.3µs   270012.        0B     27.0
## 2 part_1_improved(sample, 2020)   59.1µs   65.3µs    14784.        0B     17.0
```

For me, the improved algorithm actually takes longer on the sample data! This is because the improved algorithm has to
sort the data, which is costly, and then it performs far more comparisons per iteration.

However, when we have more data, these extra steps lead to big improvements, as can be seen when running with the actual
data.


```r
bench::mark(part_1_naive(actual, 2020),
            part_1_improved(actual, 2020))
```

```
## # A tibble: 2 x 6
##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
##   <bch:expr>                    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
## 1 part_1_naive(actual, 2020)       823µs  839.1µs     1172.        0B      0  
## 2 part_1_improved(actual, 2020)     85µs   89.5µs    10614.    1.66KB     14.9
```

The improved algorithm was roughly 10x faster for me on the actual data.

## Part 2

We can alter the naive approach from part 1 by adding in an extra for loop.


```r
part_2_naive <- function(input, target) {
  if (length(input) == 0) stop("empty list")
  
  for (i in 1:(length(input) - 2)) {
    for (j in (i + 1):(length(input) - 1)) {
      for (k in (j + 1):length(input)) {
        if (input[[i]] + input[[j]] + input[[k]] == target)
          return (prod(input[c(i, j, k)]))
      }
    }
  }
  NULL
}
```

Again, we can test that this method works on the sample.


```r
part_2_naive(sample, 2020) == 241861950
```

```
## [1] TRUE
```

It's much harder to adapt the improved algorithm though. My best approach involves a [binary search](https://en.wikipedia.org/wiki/Binary_search_algorithm).

This function takes a sorted array, a target value, a current index into the array, and the current min/max extents to
search. If the value at position i in array is less than target, we look in the left half of the array, chopping it in
half by reducing max. Likewise, if the value is greater than target, we look in the right half of the array, chopping it
in half by increasing min.


```r
binary_search <- function(arr, target, i, min, max) {
  if (arr[[i]] == target) {
    return(i)
  } else if (target < arr[[i]]) {
    max <- i
    ni <- (min + i) %/% 2
  } else {
    min <- i
    ni <- (max + i) %/% 2
  }
  
  if (ni == i) {
    NULL
  } else {
    binary_search(arr, target, ni, min, max)
  }
}
```

Our improved algorithm for part 2 involves creating two for loops to search from the end of the (sorted) input. We then
remove the third loop by using a binary search to find the target value quicker.

In the two for loops we first calculate the value of the adding these two items together, quickly checking to see if
there is a possible solution (and exiting early if not). Then, we perform a binary search on the rest of the array for
the amount required to make `v` equal `target` (2020). If we find the value, then we simply return the results. Else we
continue the search by looping.


```r
part_2_improved <- function(input, target) {
  if (length(input) == 0) stop("empty list")
  
  input <- sort(input)
  
  for (j in length(input):3) {
    for (i in (j - 1):2) {
      v <- sum(input[c(i, j)])
      # early termination
      if (v >= target) next()
      if (v + input[[i - 1]] < target) next()
      
      # binary search for a suitable value
      k <- binary_search(input, target - v, (i + 1) %/% 2, 1, (i - 1))
      if (!is.null(k)) {
        return (prod(input[c(i, j, k)]))
      }
    }
  }
  
  NULL
}
```

We can once again test that our method works on the sample data:


```r
part_2_improved(sample, 2020) == 241861950
```

```
## [1] TRUE
```

We can now run both approaches to see what the result is:


```r
part_2_naive(actual, 2020)
```

```
## [1] 61515678
```

```r
part_2_improved(actual, 2020)
```

```
## [1] 61515678
```

### Is our improved algorithm better this time?

Once again, we can benchmark the two approaches.


```r
bench::mark(part_2_naive(actual, 2020),
            part_2_improved(actual, 2020))
```

```
## # A tibble: 2 x 6
##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
##   <bch:expr>                    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
## 1 part_2_naive(actual, 2020)     129.7ms  129.8ms      7.66        0B     2.55
## 2 part_2_improved(actual, 2020)   14.9ms   15.1ms     63.1     1.66KB   505.
```

On my machine the improved approach is again about 10x quicker.

## Extra: Implementing in Python

I wanted to have a go at implementing my improved algorithm in python, but we can take advantage of sets to get even
better performance and do away with the need to sort the list and perform binary search (asking if a value is in a set
is a constant time operation in python - it's near instantaneous).


```r
library(reticulate)
```


```python
def part_2_py(input_values, target):
  # convert the 
  s = { i for i in input_values }
  
  # outer loop: we need to have at least 3 items to check
  while len(s) > 2:
    # remove a value from the set
    j = s.pop()
    # take a copy of the set: if we don't find a solution for j then we need to
    # continue with the set as it was at this point
    t = s.copy()
    while len(t) > 1:
      # remove a value from the copied set
      i = t.pop()
      # the value to reach the target
      k = target - i - j
      # is k in the set? I.e. does i + j + k == target?
      if k in t:
        return i * j * k
  # no solution, return none
  return none
```



```r
bench::mark(part_2_improved(actual, 2020),
            py$part_2_py(actual, 2020))
```

```
## # A tibble: 2 x 6
##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
##   <bch:expr>                    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
## 1 part_2_improved(actual, 2020)   14.8ms  15.41ms      63.4    1.66KB     36.7
## 2 py$part_2_py(actual, 2020)       1.2ms   1.28ms     751.    17.45KB      0
```
