# Jurassic Jigsaw



This is my attempt to solve [Day 20](https://adventofcode.com/2020/day/20).


```r
process_file <- function(file) {
  f <- read_file(file) %>%
    str_trim() %>%
    str_remove_all("\r") %>%
    str_split("\n\n") %>%
    pluck(1) %>%
    str_split("\n")
  
  t <- f %>%
    map(1) %>%
    str_extract("\\d+")
  
  f %>%
    set_names(t) %>%
    map(~ str_extract_all(.x[-1], ".", simplify = TRUE))
}

sample <- process_file("samples/day_20_sample.txt")
actual <- process_file("inputs/day_20_input.txt")
```

## Part 1

We can solve part 1 by looking which pieces have edges that don't have any matches to other pieces.


```r
get_edges <- function(.x) c(
  paste(.x[ 1, 1:10], collapse = ""),
  paste(.x[ 1, 10:1], collapse = ""),
  paste(.x[10, 1:10], collapse = ""),
  paste(.x[10, 10:1], collapse = ""),
  paste(.x[1:10,  1], collapse = ""),
  paste(.x[10:1,  1], collapse = ""),
  paste(.x[1:10, 10], collapse = ""),
  paste(.x[10:1, 10], collapse = "")
)

find_corners <- function(edges) {
  names(edges)[imap(edges, function(.x, .i) {
    other_edges <- flatten_chr(edges[names(edges) != .i])
    
    length(other_edges[other_edges %in% .x])
  }) == 4]
}
```

We can now simply find the corner pieces for our input.


```r
part_1 <- function(input) {
  edges <- map(input, get_edges)
  corners <- find_corners(edges)
  prod(as.numeric(corners))
}
```

We can run this function with the sample data:


```r
part_1(sample) == 20899048083289
```

```
## [1] TRUE
```

We can now run this with our actual data:


```r
part_1(actual)
```

```
## [1] 174206308298779
```

## Part 2

We now need to fully construct the image. First we can produce a function which takes a piece and returns all of the
8 reflections and rotations of that image (identity, 90/180/270 degree rotations, horizontal/vertical reflections and
the corner-to-corner reflections).


```r
get_rotations_and_reflections <- function(i) {
  rot90 <- function(m) {
    matrix(m[matrix(c(rep(10:1, each = 10),
                      rep(1:10, 10)),
                    ncol = 2)],
           ncol = 10,
           nrow = 10)
  }
  
  ret <- list(i = i)
  
  ret$a <- rot90(i)                   #  90 deg
  ret$b <- rot90(ret$a)               # 180 deg
  ret$c <- rot90(ret$b)               # 270 deg
  ret$d <- i[1:10, 10:1]              # horizontal reflection
  ret$e <- i[10:1, 1:10]              # vertical reflection
  ret$f <- rot90(rot90(rot90(ret$d))) # TLBR reflection
  ret$g <- rot90(rot90(rot90(ret$e))) # BLTR reflection

  ret
}
```

We also can create a function like the function for finding corners to find the outside edges.


```r
find_outside_edges <- function(edges) {
  names(edges)[imap(edges, function(.x, .i) {
    other_edges <- flatten_chr(edges[names(edges) != .i])
    
    length(other_edges[other_edges %in% .x])
  }) == 6]
}
```

Solving part 2 is now pretty tedious. First, we find the corners and outside edges. We select a corner and place it in
the top left corner.

Then, we iterate from left to right on the top row finding the single piece that matches (hoping that there are unique
pieces and we don't need to implement a backtracking algorithm). We then can find the next corner piece and repeat for
for the other edges.

Once we have found the edges, we can fill in the inside of the image, and then we can construct the image without the
borders.

Then we can simply move from left to right, top to bottom scanning for the sea monsters. If we find a sea monster we
can decrease a counter.

We simply return the counter as the final result.


```r
part_2 <- function(input) {
  size <- sqrt(length(input))
  edges <- map(input, get_edges)
  corners <- find_corners(edges)
  outside_edges <- find_outside_edges(edges)
  
  inside_pieces <- names(edges) %>%
    discard(~ .x %in% c(corners, outside_edges))
  
  all_orientations <- map(input, get_rotations_and_reflections)
  
  full_map_pieces <- matrix("", nrow = size, ncol = size)
  full_map_orientations <- matrix("", nrow = size, ncol = size)
  
  # complete top row
  
  # pick top left corner, choose first corner
  tl <- corners[[1]]
  # remove this corner
  corners <- corners[-1]

  tl_outside_edges <- edges[[tl]] %>%
    discard(~ .x %in% flatten_chr(edges[names(edges) != tl]))
  
  # we will find two options here, one will just be a reflecion in the diagonal
  # so we can discard
  tlo <- all_orientations[[tl]] %>%
    keep(~ paste(.x[1,], collapse = "") %in% tl_outside_edges) %>%
    keep(~ paste(.x[,1], collapse = "") %in% tl_outside_edges) %>%
    names() %>%
    pluck(1)
  
  full_map_pieces[1, 1] <- j <- tl
  full_map_orientations[1, 1] <- k <- tlo
  
  # now, fill in top row: assumes a single result is found
  for (i in 2:(size - 1)) {
    t <- paste(all_orientations[[j]][[k]][,10], collapse = "")
    
    r <- all_orientations[outside_edges] %>%
      map_depth(2, ~paste(.x[,1], collapse = "")) %>%
      map(keep, ~.x == t) %>%
      discard(~length(.x) == 0)
    
    stopifnot(length(r) == 1, length(r[[1]]) == 1)
    
    full_map_pieces[1, i] <- j <- names(r)
    full_map_orientations[1, i] <- k <- names(r[[1]])
    
    # remove edge
    outside_edges <- outside_edges[outside_edges != j]
  }
  
  # add in top right corner
  t <- paste(all_orientations[[j]][[k]][,10], collapse = "")
  r <- all_orientations[corners] %>%
      map_depth(2, ~paste(.x[,1], collapse = "")) %>%
      map(keep, ~.x == t) %>%
      discard(~length(.x) == 0)
  
  stopifnot(length(r) == 1, length(r[[1]]) == 1)
    
  full_map_pieces[1, size] <- j <- names(r)
  full_map_orientations[1, size] <- k <- names(r[[1]])
  
  # remove corner
  corners <- corners[corners != j]
  
  # yuck, repeat for right, then bottom, then bottom
  for (i in 2:(size - 1)) {
    t <- paste(all_orientations[[j]][[k]][10,], collapse = "")
    
    r <- all_orientations[outside_edges] %>%
      map_depth(2, ~paste(.x[1,], collapse = "")) %>%
      map(keep, ~.x == t) %>%
      discard(~length(.x) == 0)
    
    stopifnot(length(r) == 1, length(r[[1]]) == 1)
    
    full_map_pieces[i, size] <- j <- names(r)
    full_map_orientations[i, size] <- k <- names(r[[1]])
    
    # remove edge
    outside_edges <- outside_edges[outside_edges != j]
  }
  
  # add in bottom right corner
  t <- paste(all_orientations[[j]][[k]][10,], collapse = "")
  r <- all_orientations[corners] %>%
      map_depth(2, ~paste(.x[1,], collapse = "")) %>%
      map(keep, ~.x == t) %>%
      discard(~length(.x) == 0)
  
  stopifnot(length(r) == 1, length(r[[1]]) == 1)
    
  full_map_pieces[size, size] <- j <- names(r)
  full_map_orientations[size, size] <- k <- names(r[[1]])
  
  # remove corner
  corners <- corners[corners != j]
  
  # do bottom row
  for (i in (size - 1):2) {
    t <- paste(all_orientations[[j]][[k]][,1], collapse = "")
    
    r <- all_orientations[outside_edges] %>%
      map_depth(2, ~paste(.x[,10], collapse = "")) %>%
      map(keep, ~.x == t) %>%
      discard(~length(.x) == 0)
    
    stopifnot(length(r) == 1, length(r[[1]]) == 1)
    
    full_map_pieces[size, i] <- j <- names(r)
    full_map_orientations[size, i] <- k <- names(r[[1]])
    
    # remove edge
    outside_edges <- outside_edges[outside_edges != j]
  }
  
  # bottom left corner is last remaining now. We just need to find it's
  # orientation
  t <- paste(all_orientations[[j]][[k]][,1], collapse = "")
  r <- all_orientations[corners] %>%
      map_depth(2, ~paste(.x[,10], collapse = "")) %>%
      map(keep, ~.x == t) %>%
      discard(~length(.x) == 0)
  
  stopifnot(length(r) == 1, length(r[[1]]) == 1)
    
  full_map_pieces[size, 1] <- j <- names(r)
  full_map_orientations[size, 1] <- k <- names(r[[1]])
  
  # finally, fill in left edge
  for (i in (size - 1):2) {
    t <- paste(all_orientations[[j]][[k]][1,], collapse = "")
    
    r <- all_orientations[outside_edges] %>%
      map_depth(2, ~paste(.x[10,], collapse = "")) %>%
      map(keep, ~.x == t) %>%
      discard(~length(.x) == 0)
    
    stopifnot(length(r) == 1, length(r[[1]]) == 1)
    
    full_map_pieces[i, 1] <- j <- names(r)
    full_map_orientations[i, 1] <- k <- names(r[[1]])
    
    # remove edge
    outside_edges <- outside_edges[outside_edges != j]
  }
  
  inside_pieces
  
  # now, fill in centre.
  for (i in 2:(size - 1)) {
    for (j in 2:(size - 1)) {
      aj <- full_map_pieces[i, j - 1] 
      ak <- full_map_orientations[i, j - 1]
      a <- paste(all_orientations[[aj]][[ak]][,10], collapse = "")
      
      bj <- full_map_pieces[i - 1, j] 
      bk <- full_map_orientations[i - 1, j]
      b <- paste(all_orientations[[bj]][[bk]][10,], collapse = "")
      
      r <- all_orientations[inside_pieces] %>%
        map(keep, ~paste(.x[,1], collapse = "") == a) %>%
        map(keep, ~paste(.x[1,], collapse = "") == b) %>%
        discard(~length(.x) == 0)
      
      stopifnot(length(r) == 1, length(r[[1]]) == 1)
      
      full_map_pieces[i, j] <- names(r)
      full_map_orientations[i, j] <- names(r[[1]])
      
      # remove edge
      inside_pieces <- inside_pieces[inside_pieces != full_map_pieces[i, j]]
    }
  }
  
  # we can now reconstruct the image: we need to remove the border's from the images
  image <- matrix(nrow = size * 8, ncol = size * 8)
  for (i in 1:size) {
    for (j in 1:size) {
      istart <- (i - 1) * 8
      jstart <- (j - 1) * 8
      
      aj <- full_map_pieces[i, j ] 
      ak <- full_map_orientations[i, j]
      current <- all_orientations[[aj]][[ak]][2:9, 2:9]
      image[istart:(istart + 7) + 1, jstart:(jstart + 7) + 1] <- current
    }
  }
  
  sea_monster <- local({
    t <- c("                  # ",
           "#    ##    ##    ###",
           " #  #  #  #  #  #   ") %>%
      str_extract_all(".", simplify = TRUE)
    which(t == "#", TRUE) - 1
  })

  # iterate over the image, count how many cells are sea monsters
  count <- sum(image == "#")
  # hard code in size of sea monster
  for (i in 1:(nrow(image) - 3)) {
    for (j in 1:(ncol(image) - 20)) {
      ix <- sea_monster
      
      ix[,1] <- ix[,1] + i
      ix[,2] <- ix[,2] + j
      
      t <- image[ix] == "#"
      if (all(t)) {
        count <- count - sum(t) 
      }
    }
  }
  
  count
}
```

We can run the function now on the sample:


```r
part_2(sample)
```

```
## [1] 273
```

And we can run for our actual data:


```r
part_2(actual)
```

```
## [1] 2409
```

This felt rather tedious and procedural, perhaps there is a neater solution. Improvements could be made to my code by
extracting some of the repeated logic into functions.

---

*Elapsed Time: 2.296s*
