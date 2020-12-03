# Toboggan Trajectory



This is my attempt to solve [Day 3](https://adventofcode.com/2020/day/3).


```r
sample <- read_lines("samples/day_03_sample.txt")
actual <- read_lines("inputs/day_03_input.txt")
```

## Part 1

I think it will be easier to convert the map to a 0-1 matrix to solve today's problem. We won't need to infinitely
replicate the pattern to the right as we can just use modular arithmetic to index into the matrix.


```r
input_to_matrix <- function(input) {
  input %>%
    # extract each character from the input
    str_extract_all(".", simplify = TRUE) %>%
    # if a value is "#" then TRUE, else FALSE
    `==`("#") %>%
    # convert the TRUE's to 1, FALSE's to 0
    apply(c(1,2), as.integer)
}
sample_matrix <- input_to_matrix(sample)
actual_matrix <- input_to_matrix(actual)
sample_matrix
```

```
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
##  [1,]    0    0    1    1    0    0    0    0    0     0     0
##  [2,]    1    0    0    0    1    0    0    0    1     0     0
##  [3,]    0    1    0    0    0    0    1    0    0     1     0
##  [4,]    0    0    1    0    1    0    0    0    1     0     1
##  [5,]    0    1    0    0    0    1    1    0    0     1     0
##  [6,]    0    0    1    0    1    1    0    0    0     0     0
##  [7,]    0    1    0    1    0    1    0    0    0     0     1
##  [8,]    0    1    0    0    0    0    0    0    0     0     1
##  [9,]    1    0    1    1    0    0    0    1    0     0     0
## [10,]    1    0    0    0    1    1    0    0    0     0     1
## [11,]    0    1    0    0    1    0    0    0    1     0     1
```

Now that we have 0-1 matrices for the sample and actual data, we can consider how to solve the puzzle. For each row that
we move down the matrix, we need to move 3 to the right (starting from `[1,1]`). Now, the pattern repeats infinitely to
the right; however, we do not need to repeat the matrix at all. We can simply "wrap around" back to 1 when we reach the
12th column.

In this case, we want to start from 1, then move 3 to the right to 4, then to 7 and 10. But after that we would land at
13, which is the same as being at position 2. We can achieve this using the "remainder" (`%%`) operator. This is 0
based however, so we need to subtract 1 from the row number and add 1 at the end:


```r
(3 * (1:11 - 1)) %% 11 + 1
```

```
##  [1]  1  4  7 10  2  5  8 11  3  6  9
```

We can now build a function to work out which "trees" we would land on by traversing the matrix.


```r
part_1 <- function(input) {
  # create a matrix the same size as input, first filled with 0's
  m <- input * 0
  # but replace with a 1 where we would land on that row
  for (i in 1:nrow(m)) m[i, (3 * (i - 1)) %% ncol(m) + 1] <- 1
  
  # just standard multiplication, not matrix multiplication 
  sum(input * m)
}
```

We can check that this function matches the sample provided:


```r
part_1(sample_matrix) == 7
```

```
## [1] TRUE
```

We can now run the part 1 function on the actual data:


```r
part_1(actual_matrix)
```

```
## [1] 151
```

## Part 2

We now need to adapt our function from part 1 to work with different step sizes.


```r
part_2 <- function(input, down, right) {
  # create a matrix the same size as input, first filled with 0's
  m <- input * 0
  # but replace with a 1 where we would land on that row
  for (i in seq(1, nrow(m), down)) {
    # our for loop now skips certain rows, but we need to alter our index to be
    # based on the current step that we are on: so we divide (i - 1) by the down
    # step size
    m[i, (right * (i - 1) / down) %% ncol(m) + 1] <- 1
  }
  
  # just standard multiplication, not matrix multiplication. because our trees
  # are encoded by a 1, and the positions we land are encoded by a 1, we will
  # find the trees that we encounter iff both positions in input and m are 1.
  # The value in the resultant matrix will be a 1 in those positions,
  # otherwise 0. We can then simply sum the matrix.
  sum(input * m)
}
```

We can check that this function matches the sample provided:


```r
all(
  part_2(sample_matrix, 1, 1) == 2,
  part_2(sample_matrix, 1, 3) == 7,
  part_2(sample_matrix, 1, 5) == 3,
  part_2(sample_matrix, 1, 7) == 4,
  part_2(sample_matrix, 2, 1) == 2
)
```

```
## [1] TRUE
```

We can now run the part 1 function on the actual data:


```r
prod(
  part_2(actual_matrix, 1, 1),
  part_2(actual_matrix, 1, 3),
  part_2(actual_matrix, 1, 5),
  part_2(actual_matrix, 1, 7),
  part_2(actual_matrix, 2, 1)
)
```

```
## [1] 7540141059
```