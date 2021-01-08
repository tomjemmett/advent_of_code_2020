# Crab Cups



This is my attempt to solve [Day 23](https://adventofcode.com/2020/day/23).


```r
sample <- as.numeric(str_extract_all("389125467", ".")[[1]])
actual <- as.numeric(str_extract_all("562893147", ".")[[1]])
```

## Part 1

To solve today's puzzle we can use a vector where each position's value indicates the next index to select.

First we create our ["linked list"](en.wikipedia.org/wiki/Linked_list) of values, then create a "current" pointer (where
we are going to start in the list). Then, we simply iterate `n` times.


```r
play <- function(input, n) {
  size <- length(input)
  
  # generate the linked list
  ll <- c(2:size, 1)
  j <- size
  for (i in input) {
    ll[[j]] <- i
    j <- i
  }
  ll[[j]] <- input[[1]]
  cur <- input[[1]]
  
  # run our iterations
  for (i in 1:n) {
    # get the picked up values
    picked_up_a <- ll[[cur]]
    picked_up_b <- ll[[picked_up_a]]
    picked_up_c <- ll[[picked_up_b]]
    # "remove" these values from the list by pointing the current index to be
    # the value just after the 3rd picked up cup
    ll[[cur]] <- ll[[picked_up_c]]
    # calculate the destination value
    destination <- cur - 1
    if (destination == 0) {
      destination <- size
    }
    while (destination == picked_up_a ||
           destination == picked_up_b ||
           destination == picked_up_c) {
      destination <- destination - 1
      if (destination == 0) {
        destination <- size
      }
    }
    # insert the picked up cups just after the destination
    t <- ll[[destination]]
    ll[[destination]] <- picked_up_a
    ll[[picked_up_c]] <- t
    # move to the next cups
    cur <- ll[[cur]]
  }
  # return the linked list
  ll
}
```

We can now use our `play()` function to solve part 1.


```r
part_1 <- function(input, n) {
  # run the function on our input
  r <- play(input, n)
  # create an out vector the length of our input, excluding the "1" value
  out <- numeric(length(input) - 1)
  # now, iterate from the "1" position through the linked list, adding the
  # values to the output and selecting the next item in the linked list
  j <- 1
  for (i in seq_along(out)) {
    out[[i]] <- r[[j]]
    j <- r[[j]]
  }
  # return as a string
  paste(out, collapse = "")
}
```

We can test that our function works against the sample data:


```r
part_1(sample,  10) == "92658374"
```

```
## [1] TRUE
```

```r
part_1(sample, 100) == "67384529"
```

```
## [1] TRUE
```

And we can run our function against the actual data:


```r
part_1(actual, 100)
```

```
## [1] "38925764"
```

## Part 2

We don't need to modify our play function, merely we need to add in the remaining cups from 10 to 1,000,000.


```r
part_2 <- function(input) {
  input <- c(input, (max(input) + 1):1000000)
  r <- play(input, 10000000)
  
  a <- r[[1]]
  b <- r[[a]]
  
  a * b
}
```

We can test that this function works on the provided sample:


```r
part_2(sample) == 149245887792
```

```
## [1] TRUE
```

And we can run the function on our actual data:


```r
part_2(actual)
```

```
## [1] 131152940564
```

---

*Elapsed Time: 9.744s*
