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

# Extra: Is there a path that minimises the number of trees you encounter?

First, let's define some additional constraints. We must consider movements that are unique; for instance, going 2 down
and 2 right is the same as 1 down, 1 right. In this case we will only check 1 down and 1 right.

We also know that going 0 to the right is the same as going the number of columns to the right. So, we consider a
maximum step right that is 1 less than the number of columns. Likewise, down must be between 1 and the number of rows.

We can run our algorithm for any valid combination of movements down and right that match the constraints above. First,
let's consider what happens when we don't go right, but we go straight down.


```r
part_2(actual_matrix, 1, 0)
```

```
## [1] 105
```

Now, what happens if we always go 1 right?


```r
map_dbl(1:nrow(actual_matrix), part_2, input = actual_matrix, right = 1)
```

```
##   [1] 103  59  29  27  18  13  23  12  10  10   9   9   6   6   6   4   7   3
##  [19]   9   8   8   1   2   3   3   3   2   1   3   7   1   3   2   3   0   1
##  [37]   5   1   4   1   3   1   2   2   2   1   1   3   3   1   1   3   2   0
##  [55]   2   1   1   2   1   2   2   1   2   4   0   2   1   0   1   0   1   0
##  [73]   3   2   2   1   2   1   0   1   2   0   1   2   2   1   0   1   0   1
##  [91]   1   1   1   1   0   0   0   1   1   0   3   0   2   1   1   0   1   1
## [109]   1   1   1   0   1   0   0   0   1   0   0   0   0   0   0   0   0   1
## [127]   0   0   0   0   0   0   1   0   1   0   0   0   1   1   0   0   1   0
## [145]   1   1   1   0   1   0   0   0   1   0   0   0   1   0   0   1   1   0
## [163]   0   0   1   0   1   0   0   0   1   0   0   0   0   0   0   0   0   0
## [181]   0   1   0   0   1   0   0   0   0   0   1   0   0   1   0   0   0   1
## [199]   0   0   0   0   0   0   1   1   0   0   0   1   0   0   0   0   0   0
## [217]   0   0   0   1   0   0   0   0   0   0   0   0   1   1   1   0   0   1
## [235]   0   0   0   1   0   0   0   1   0   0   1   0   0   0   0   1   0   1
## [253]   1   1   0   0   0   0   1   1   1   0   0   1   0   0   1   1   0   1
## [271]   0   0   0   0   0   1   0   0   0   0   0   0   1   0   1   0   0   0
## [289]   1   0   0   0   1   0   0   0   1   0   0   1   0   0   1   1   1   0
## [307]   1   0   0   1   1   0   0   1   0   0   0   0   1   0   0   0   0
```

We can see a number of options here where we can get to the bottom without hitting any trees. We can't get any better
than 0, so we may as well give up now rather than try all of the other combinations!

The other combinations would be going just 1 down at each step, and then all of the combinations of right and down such
that the greatest common divisor of right and down is 1. This would give us all of the movements that don't violate the
constraint above of a non-unique movement. For instance, 6 right, 2 down has a greatest common divisor of 2, and this
combination is the same as 3 right, 1 down.
