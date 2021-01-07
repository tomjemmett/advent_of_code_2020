# Lobby Layout



This is my attempt to solve [Day 24](https://adventofcode.com/2020/day/24).


```r
sample <- read_lines("samples/day_24_sample.txt")
actual <- read_lines("inputs/day_24_input.txt")
```

## Part 1

First let's create a function that will iterate over the input and update a list of tiles. We use a coordinate system
based on a [cube](https://www.redblobgames.com/grids/hexagons/) and an array to store the values. We assume that a grid
size of -100 to +100 in all directions will be sufficient.


```r
# set the directions that we move, using hex cube coordinates
directions <- list(
  e  = c( 1, -1,  0),
  se = c( 0, -1,  1),
  sw = c(-1,  0,  1),
  w  = c(-1,  1,  0),
  nw = c( 0,  1, -1),
  ne = c( 1,  0, -1)
)

get_tiles <- function(input) {
  # each tile will be stored as 0 for white, 1 for black
  tiles <- array(0, dim = c(200, 200, 200))
  # iterate over the input, splitting each line into individual characters
  for (line in str_extract_all(input, ".")) {
    # we always start from (100, 100, 100) - the centre of our array
    p <- matrix(100, ncol = 3)
    # create an index into the current input line and iterate over it
    i <- 1
    while (i <= length(line)) {
      # get the current item in the line
      d <- line[[i]]
      # add in the next character if d is "s" or "n"
      if (d == "s" | d == "n") {
        i <- i + 1
        d <- paste0(d, line[[i]])
      }
      # update the current position
      p <- p + directions[[d]]
      i <- i + 1
    }
    # flip the tile
    tiles[p] <- 1 - tiles[p]
  }
  # return the tiles
  tiles
}
```

We can use this function to solve part 1, we simply need to sum the results. But, because we return a list we first
need to flatten it to a double vector.


```r
part_1 <- function(input) {
  input %>%
    get_tiles() %>%
    sum()
}
```

We can test that this function works against the provided sample:


```r
part_1(sample) == 10
```

```
## [1] TRUE
```

And we can run this function against our actual data:


```r
part_1(actual)
```

```
## [1] 394
```

## Part 2

We can solve part 2 by first using our `get_tiles()` function to get our initial state, then looping `n` times to
calculate the next state.

First, we find which tiles are currently black, and then we find the neighbours of the black tiles. We consider what the
next state of these tiles should be and save these values till later.

Next, we take the list of neighbours and see how many times these neighbours appear. If they appear exactly twice then
this tile will become black.

With these two pieces of information we update the array and repeat.


```r
part_2 <- function(input, n) {
  tiles <- get_tiles(input)
  
  for (i in 1:n) {
    # find the tiles which are currently black
    is_black <- which(tiles == 1, TRUE)
    # find the neighbours of these black tiles
    neighbours <- map(array_tree(is_black), function(v) {
      t(map_dfr(directions, ~flatten_dbl(v) + .x))
    })
    # for each of the black tiles, work out if it stays black or turns white
    # we make a simplification here, if the sum is 2 we will also handle this
    # in the white to black step. Multiplying by 1 will convert TRUE to 1 and
    # FALSE to 0
    new_v <- map_dbl(neighbours, ~sum(tiles[.x])) == 1 * 1
    
    # the only white tiles which can become black are those which are neighbours
    # of black tiles. If a neighbour tile appears exactly twice then it will
    # turn to black, so we can simply turn our neighbours to a single dataframe
    # and count how many times each set of coordinates appear. Keep only the
    # rows that appear twice, then turn the coordinates back to a matrix so we
    # can use as an index back into tiles
    white_to_black <- neighbours %>%
      map_dfr(as.data.frame) %>%
      count(V1, V2, V3) %>%
      filter(n == 2) %>%
      select(-n) %>%
      as.matrix()
    
    # update the tiles matrix
    tiles[is_black] <- new_v
    tiles[white_to_black] <- 1
  }
  
  # return the results
  sum(tiles)
}
```

We can test this function on the sample data:


```r
part_2(sample, 100) == 2208
```

```
## [1] TRUE
```

And we can run this function on the actual data:


```r
part_2(actual, 100)
```

```
## [1] 4036
```

---

*Elapsed Time: 3.005s*
