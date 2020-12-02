# Report Repair



This is my attempt to solve [Day 1](https://adventofcode.com/2020/day/1).


```r
sample <- c(1721, 979, 366, 299, 675, 1456)
actual <- read_lines("day_01_input.txt") %>% as.integer()
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
## [90m# A tibble: 2 x 6[39m
##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
##   [3m[90m<bch:expr>[39m[23m                    [3m[90m<bch:tm>[39m[23m [3m[90m<bch:tm>[39m[23m     [3m[90m<dbl>[39m[23m [3m[90m<bch:byt>[39m[23m    [3m[90m<dbl>[39m[23m
## [90m1[39m part_1_naive(sample, 2020)      2.93µs   3.62µs   [4m2[24m[4m3[24m[4m6[24m802.        0B     23.7
## [90m2[39m part_1_improved(sample, 2020)  62.02µs  65.72µs    [4m1[24m[4m4[24m823.        0B     17.0
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
## [90m# A tibble: 2 x 6[39m
##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
##   [3m[90m<bch:expr>[39m[23m                    [3m[90m<bch:tm>[39m[23m [3m[90m<bch:tm>[39m[23m     [3m[90m<dbl>[39m[23m [3m[90m<bch:byt>[39m[23m    [3m[90m<dbl>[39m[23m
## [90m1[39m part_1_naive(actual, 2020)     818.7µs  830.6µs     [4m1[24m185.        0B     2.02
## [90m2[39m part_1_improved(actual, 2020)   82.5µs   88.7µs    [4m1[24m[4m1[24m012.    1.66KB    14.8
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
## [90m# A tibble: 2 x 6[39m
##   expression                         min   median `itr/sec` mem_alloc `gc/sec`
##   [3m[90m<bch:expr>[39m[23m                    [3m[90m<bch:tm>[39m[23m [3m[90m<bch:tm>[39m[23m     [3m[90m<dbl>[39m[23m [3m[90m<bch:byt>[39m[23m    [3m[90m<dbl>[39m[23m
## [90m1[39m part_2_naive(actual, 2020)       127ms  132.1ms      7.62        0B     2.54
## [90m2[39m part_2_improved(actual, 2020)     15ms   16.1ms     61.5     1.66KB   472.
```

On my machine the improved approach is again about 10x quicker, though there are more memory allocations and GC
(garbage collection's) happening, so the total execution time is not quite 10x.
