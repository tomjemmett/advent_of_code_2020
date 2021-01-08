# Rain Risk



This is my attempt to solve [Day 12](https://adventofcode.com/2020/day/12).


```r
sample <- read_lines("samples/day_12_sample.txt")
actual <- read_lines("inputs/day_12_input.txt")
```

## Part 1

Today's task could be a tedious series of if else statements. Instead I will use Object-Orientated Programming and the
[s3](https://adv-r.hadley.nz/s3.html) class system. I will strip the initial input to just be the number, but I will
give each of the inputs a class of "N", "E", "W", "S", "F", "R", or "L".


```r
split_instructions <- function(input) {
  transpose(
    list(
      d = str_sub(input, 1, 1),
      n = as.integer(str_sub(input, 2))
    )
  ) %>%
    map(function(.x) {
      structure(.x$n, class = .x$d)
    })
}

isample <- split_instructions(sample)
iactual <- split_instructions(actual)
isample
```

```
## [[1]]
## [1] 10
## attr(,"class")
## [1] "F"
## 
## [[2]]
## [1] 3
## attr(,"class")
## [1] "N"
## 
## [[3]]
## [1] 7
## attr(,"class")
## [1] "F"
## 
## [[4]]
## [1] 90
## attr(,"class")
## [1] "R"
## 
## [[5]]
## [1] 11
## attr(,"class")
## [1] "F"
```

First, let's define the initial state:


```r
initial_state <- list(direction = "E", position = c(0, 0))
```

We now need to create a function called that will run an instruction: I will call this function `inst`. It will take as
argument the object itself (`n`), this will contain the number from the input. It will also take the current state.


```r
inst <- function(n, state) {
  UseMethod("inst")
}
```

Now we need to implement a method for each of the classes. Let's start with the N/E/S/W classes. These methods are
pretty simple, we just need to add on `n` to the current position.


```r
# create a helper function that the other functions use
inst_NESW_helper <- function(state, p) {
  state$position <- state$position + p
  state
}

inst.N <- function(n, state) inst_NESW_helper(state, c( n,  0))
inst.E <- function(n, state) inst_NESW_helper(state, c( 0,  n))
inst.S <- function(n, state) inst_NESW_helper(state, c(-n,  0))
inst.W <- function(n, state) inst_NESW_helper(state, c( 0, -n))
```

The L/R functions are slightly more tricky, but, looking at the actual data there are only 3 different angles that are
turned:


```r
actual %>% str_subset("L|R") %>% str_remove("L|R") %>% unique()
```

```
## [1] "90"  "180" "270"
```

We can create our function now. This time, we our helper function will just rotate the direction that we are facing.


```r
inst_LR_helper <- function(state, r) {
  directions <- c("N", "E", "S", "W")
  current <- which(state$direction == directions) - 1
  next_direction <- (current + r / 90) %% 4
  
  state$direction = directions[[next_direction + 1]]
  state
}

# L turns anti-clockwise, so we can turn it into a clockwise rotation
inst.L <- function(n, state) inst_LR_helper(state, 360 - n)
inst.R <- function(n, state) inst_LR_helper(state, n)
```

Finally we can create our function for moving forward. In this case we will take the value from `n`, but update the
class to be that of the current direction from `state`.


```r
inst.F <- function(n, state) {
  nn <- structure(n, class = state$direction)
  inst(nn, state)
}
```

Now we have all the parts we can build our function to solve part 1. We start with an initial state, and keep calling
a function on each successive value in the input with the current state. To do this we can use the reduce function.


```r
solve <- function(input, state = initial_state) {
  final_state <- reduce(
    input,
    .init = initial_state,
    function (state, instruction) {
      inst(instruction, state)
    }
  )
  
  sum(abs(final_state$position))
}
```

Checking the sample data:


```r
solve(isample) == 25
```

```
## [1] TRUE
```

We can now run with the actual data:


```r
solve(iactual)
```

```
## [1] 1032
```

## Part 2

We can now update our instruction functions, our solver should still work.

First we need to update our initial state.


```r
initial_state <- list(waypoint = c(1, 10), position = c(0, 0))
```

We can cheat a bit on the N/E/S/W functions: we can just change the update position helper function to instead update
the waypoint.


```r
inst_NESW_helper <- function(state, p) {
  state$waypoint <- state$waypoint + p
  state
}
```

Likewise, we just need to update the L/R helper function. We can use
[matrix multiplication](https://en.wikipedia.org/wiki/Rotation_matrix#Common_rotations) to rotate our waypoint.


```r
inst_LR_helper <- function(state, r) {
  # r is either 90, 180, 270, and it a clockwise rotation
  rot_mat <- matrix(
    if (r == 90) {
      c(0, 1, -1, 0)
    } else if (r == 180) {
      c(-1, 0, 0, -1)
    } else {
      c(0, -1, 1, 0)
    },
    nrow = 2
  )
  
  state$waypoint <- (rot_mat %*% state$waypoint)[,1]
  state
}
```

We now just need to update the F function.


```r
inst.F <- function(n, state) {
  w <- state$waypoint
  s <- state$position
  
  state$position <- w * n + s
  state
}
```

We should now be in a position to run our `solve` function again. Checking the sample:


```r
solve(isample) == 286
```

```
## [1] TRUE
```

We can now run with the actual data:


```r
solve(iactual)
```

```
## [1] 156735
```

---

*Elapsed Time: 0.312s*
