# Seating System



This is my attempt to solve [Day 11](https://adventofcode.com/2020/day/11).


```r
sample <- read_lines("samples/day_11_sample.txt")
actual <- read_lines("inputs/day_11_input.txt")
```

## Part 1

We are given that in our input:

- `.` is the floor
- `L` is an empty seat
- `#` is an occupied seat

It will make our life simpler to add a border of empty seats round our inputs, and convert the input to a matrix of
individual characters


```r
process_input <- function(input) {
  dl <- paste(rep(".", str_length(input[[1]])), collapse = "")
  m <- paste0(".", c(dl, input, dl), ".") %>%
    str_extract_all(".", simplify = TRUE)
  
  # give this matrix an s3 class of "day11"
  structure(m, class = c("day11", class(m)))
}

# implement a print method for our day11 class
print.day11 <- function(x, ...) {
  cat(paste(apply(x, 1, paste, collapse = ""), collapse = "\n"), "\n")
}

psample <- process_input(sample)
pactual <- process_input(actual)
```

We can now have a quick look at our sample input:


```r
psample
```

```
## ............
## .L.LL.LL.LL.
## .LLLLLLL.LL.
## .L.L.L..L...
## .LLLL.LL.LL.
## .L.LL.LL.LL.
## .L.LLLLL.LL.
## ...L.L......
## .LLLLLLLLLL.
## .L.LLLLLL.L.
## .L.LLLLL.LL.
## ............
```

Now we can build a function to run n iteration's:


```r
p1_run_iterations <- function(input, n = 1, count = 0) {
  # this is a recursive function, when n is less than 1 stop iterating and
  # return whatever input is given
  if (n < 1) return (list(input = input, count = count))
  
  # take a copy of the current state - we will modify this copied state and
  # return it for the next iteration
  next_state <- input
  for (r in 2:(nrow(input) - 1)) {
    for (c in 2:(ncol(input) - 1)) {
      # skip this cell if it's a .
      if (input[r, c] == ".") next()
      # get adjacent seats
      adjacent <- c(input[r - 1, c - 1],
                    input[r - 1, c    ],
                    input[r - 1, c + 1],
                    input[r    , c - 1],
                    input[r    , c + 1],
                    input[r + 1, c - 1],
                    input[r + 1, c    ],
                    input[r + 1, c + 1])
      # count how many are occupied
      occupied <- sum(ifelse(adjacent == "#", 1, 0))
      
      if (input[r, c] == "L") {
        if (occupied == 0) {
          next_state[r, c] <- "#"
        }
      } else {
        # it can only be "#" now
        if (occupied >= 4) {
          next_state[r, c] <- "L"
        }
      }
    }
  }
  
  if (all(next_state == input)) {
    return (list(input = input, count = count))
  }
  
  # run the next itertion, decreasing n by 1
  p1_run_iterations(next_state, n - 1, count + 1)
}
```

We can now test our function runs as expected on the sample.


```r
p1_run_iterations(psample)
```

```
## $input
## ............
## .#.##.##.##.
## .#######.##.
## .#.#.#..#...
## .####.##.##.
## .#.##.##.##.
## .#.#####.##.
## ...#.#......
## .##########.
## .#.######.#.
## .#.#####.##.
## ............ 
## 
## $count
## [1] 1
```


```r
p1_run_iterations(psample, 2)
```

```
## $input
## ............
## .#.LL.L#.##.
## .#LLLLLL.L#.
## .L.L.L..L...
## .#LLL.LL.L#.
## .#.LL.LL.LL.
## .#.LLLL#.##.
## ...L.L......
## .#LLLLLLLL#.
## .#.LLLLLL.L.
## .#.#LLLL.##.
## ............ 
## 
## $count
## [1] 2
```

These match the examples, we just need to check it terminates correctly:


```r
p1s <- p1_run_iterations(psample, Inf)
p1s$count == 5
```

```
## [1] TRUE
```

We just need a way to count the seats occupied now:


```r
count_seats <- function(x) {
  sum(x$input == "#")
}
```

Which we can run on the variable `p1s` from above:


```r
count_seats(p1s) == 37
```

```
## [1] TRUE
```

While this does run, it's not particular fast on the actual data. We come back to part 1 later.


```r
# disabled chunk
pactual %>%
  p1_run_iterations(Inf) %>%
  count_seats()
```

## Part 2

We now need to modify our run iterations function. I am going to embed a function that will search for the first seat,
and update the tolerance from 4 to 5.

In order to speed up computation we first calculate the "first seat" found from any position. In order to remember these
seats we switch from a recursive function to use a loop. Otherwise, the function remains the same as in part 1.


```r
p2_run_iterations <- function(input, n) {
  # create a function to find the first seat that can be seen from r, c in the
  # direction rd, cd. rd / cd should be 1, -1 or 0, but both should not be 0
  find_first_seat <- function(r, c, rd, cd) {
    rr <- r
    cc <- c
    if (input[rr, cc] == ".") {
      # return a cell that will be a "."
      return (c(1, 1))
    }
    repeat {
      rr <- rr + rd
      cc <- cc + cd
      if (rr < 1 | rr > nrow(input) |
          cc < 1 | cc > ncol(input) ) {
        # return a cell that will be a "."
        return (c(1, 1))
      } else if (input[rr, cc] != ".") {
        # return the indexes
        return (c(rr, cc))
      }
    }
  }
  
  first_seats <- map(2:(nrow(input) - 1), function(r) {
    map(2:(ncol(input) - 1), function(c) {
      list(
        rd = c(-1, -1, -1,  0,  0,  1,  1,  1),
        cd = c(-1,  0,  1, -1,  1, -1,  0,  1)
      ) %>%
        pmap(find_first_seat, r = r, c = c) %>%
        discard(compose(any, is.na))
    })
  })
  
  count <- 0
  state <- input
  while (count < n) {
    # take a copy of the current state - we will modify this copied state and
    # return it for the next iteration
    next_state <- state
    
    for (r in 2:(nrow(state) - 1)) {
      for (c in 2:(ncol(state) - 1)) {
        # skip this cell if it's a .
        if (state[r, c] == ".") next()
        
        # our first seats only iterated over the "inside" range, so we need to
        # subtract 1 from the r and c index
        adjacent <- map_chr(first_seats[[r - 1]][[c - 1]],
                            ~state[.x[[1]], .x[[2]]])
        
        # count how many are occupied
        occupied <- sum(adjacent == "#")
        
        if (state[r, c] == "L") {
          if (occupied == 0) {
            next_state[r, c] <- "#"
          }
        } else {
          # it can only be "#" now
          if (occupied >= 5) {
            next_state[r, c] <- "L"
          }
        }
      }
    }
    
    if (all(next_state == state)) {
      break()
    }
    
    state <- next_state
    count <- count + 1
  }
  
  list(input = state, count = count)
}
```

We can test our function works as expected after 1 iteration:


```r
psample %>%
  p2_run_iterations(1) 
```

```
## $input
## ............
## .#.##.##.##.
## .#######.##.
## .#.#.#..#...
## .####.##.##.
## .#.##.##.##.
## .#.#####.##.
## ...#.#......
## .##########.
## .#.######.#.
## .#.#####.##.
## ............ 
## 
## $count
## [1] 1
```

And after 2 iterations:


```r
psample %>%
  p2_run_iterations(2) 
```

```
## $input
## ............
## .#.LL.LL.L#.
## .#LLLLLL.LL.
## .L.L.L..L...
## .LLLL.LL.LL.
## .L.LL.LL.LL.
## .L.LLLLL.LL.
## ...L.L......
## .LLLLLLLLL#.
## .#.LLLLLL.L.
## .#.LLLLL.L#.
## ............ 
## 
## $count
## [1] 2
```

We can use the `count_seats()` function again to test our new function works:


```r
psample %>%
  p2_run_iterations(Inf) %>%
  count_seats() == 26
```

```
## [1] TRUE
```

Again, this function runs very slowly on the actual data.


```r
# disabled chunk
pactual %>%
  p2_run_iterations(Inf) %>%
  count_seats()
```

## Solving faster

R is much better at vectorised operations, so if we could reduce the steps to summing matrices our code should run much
faster.

First, we are to treat our input as a matrix that contains either 0 for an unoccupied seat, 1 for an occupied seat, and
`NA` for the floor.

We then create functions for part 1 and for part 2 which returns 8 set's of indices for the directions that we are to
look in for seats. These index sets will be used to return a matrix the same size as the inner part of our matrix (we
ignore the border).

Part 1's function is pretty simple, we simply shift the matrix one up, one to the left, then just one up, then one up
and one to the right, etc. This function is pretty slow as we have to allocate quite a lot of memory.


```r
part_one <- function(input) {
  nr <- nrow(input)
  nc <- ncol(input)
  
  cross2(1:3, 1:3) %>%
    discard(~ .x[[1]] == 2 && .x[[2]] == 2) %>%
    map(function(.x) {
      cross_df(
        list(
          row = .x[[1]]:(nr - 3 + .x[[1]]),
          col = .x[[2]]:(nc - 3 + .x[[2]])
        )
      ) %>%
        as.matrix()
    })
}
```

Part 2 is slightly more complex as we need to find the seat according to the more complex rules.


```r
part_two <- function(input) {
  nr <- nrow(input)
  nc <- ncol(input)
  
  find_in_direction <- function(r, c, rd, cd) {
    ri <- r
    ci <- c
    repeat {
      ri <- ri + rd
      ci <- ci + cd
      
      # we have reached the boundary, exit
      if (ri < 1 | ri > nr | ci < 1 | ci > nc) {
        return (list(row = ri - rd, col = ci - cd))
      }
      # we have found a seat, return
      if (!is.na(input[ri, ci])) {
        return (list(row = ri, col = ci))
      }
    }
  }
 
  # get the indices of each cell in the inner part of the input matrix 
  ixs <- cross_df(list(r = 2:(nr - 1), c = 2:(nc - 1)))
  
  # now find the values in each direction
  list(
    pmap_dfr(ixs, find_in_direction, -1, -1),
    pmap_dfr(ixs, find_in_direction, -1,  0),
    pmap_dfr(ixs, find_in_direction, -1,  1),
    pmap_dfr(ixs, find_in_direction,  0, -1),
    pmap_dfr(ixs, find_in_direction,  0,  1),
    pmap_dfr(ixs, find_in_direction,  1, -1),
    pmap_dfr(ixs, find_in_direction,  1,  0),
    pmap_dfr(ixs, find_in_direction,  1,  1)
  ) %>%
    map(as.matrix)
}
```

We now create a solving function, which takes the data as the first argument, either `part_one()` or `part_two()` as the
second argument, and the tolerance (4 for part 1, 5 for part 2).


```r
solve <- function(x, add_mat_fn, tolerance) {
  input <- ifelse(unclass(x) == "L", 0, NA)
  
  nr <- nrow(input)
  nc <- ncol(input)
  
  add_mat_ix <- add_mat_fn(input)
  
  repeat {
    # use this to check later if our matrix has changed
    t <- input
      
    # use our matrix indices, find the values for the 8 shifted matrices, then
    # add the 8 matrices together to give us one matrix that tells us how many
    # adjacent seats are occupied
    add_mat <- add_mat_ix %>%
      map(~input[.x] %>%
            replace_na(0) %>%
            matrix(nrow = nr - 2, ncol = nc - 2)) %>%
      reduce(`+`)
    
    # find the unoccupied seats in the input
    y <- which(input == 0, arr.ind = TRUE)
    # and the occupied seats
    z <- which(input == 1, arr.ind = TRUE)
    
    # update the unoccupied seats
    input[y] <- 1 * (add_mat[y - 1] == 0)
    # update the occupied seats
    input[z] <- 1 * (add_mat[z - 1] <  tolerance)
    
    # check to see if our matrix has changed
    if (all(t == input, na.rm = TRUE)) break()
  }
  # now just return the number of occupied seats
  sum(input, na.rm = TRUE)
}
```

We can now run our new function's and see if they give us the same results as above:


```r
solve(psample, part_one, 4)
```

```
## [1] 37
```

```r
solve(pactual, part_one, 4)
```

```
## [1] 2483
```

```r
solve(psample, part_two, 5)
```

```
## [1] 26
```

```r
solve(pactual, part_two, 5)
```

```
## [1] 2285
```

---

*Elapsed Time: 5.57s*
