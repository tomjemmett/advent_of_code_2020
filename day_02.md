# Password Philosophy



This is my attempt to solve [Day 2](https://adventofcode.com/2020/day/2).


```r
sample <- c("1-3 a: abcde",
            "1-3 b: cdefg",
            "2-9 c: ccccccccc")
actual <- read_lines("day_02_input.txt")
```

## Part 1

We need to convert each string into a list that contains the following:

 - minimum number of occurrences of the character
 - maximum number of occurrences of the character
 - the character that must be present
 - the password


```r
split_input <- function(input) {
  input %>%
    # first, split the string into the bit before the password, and the password
    str_split(": ") %>%
    # now, we can iterate over the results (each line of data)
    map(function(.x) {
      # find the min/max values from the first part of the input string .x
      mv <- .x[[1]] %>%
        # we can remove the last 2 characters of the string (the character)
        str_sub(1, -3) %>%
        # now we can split the string into the min and max values
        str_split("-") %>%
        # str_split is vectorised, but we only want the first results as we are dealing with one line at a time:
        # the first result will contain two values
        pluck(1) %>%
        # now convert these to integers
        as.integer()
      
      # we can now return the results
      list(
        min = mv[[1]],
        max = mv[[2]],
        character = str_sub(.x[[1]], -1, -1),
        password = .x[[2]]
      )
    })
}
```

We can see what our split_input function does to the sample data, looking at just the first item


```r
str(split_input(sample)[[1]])
```

```
## List of 4
##  $ min      : int 1
##  $ max      : int 3
##  $ character: chr "a"
##  $ password : chr "abcde"
```

Now we can build a function to return the valid passwords.


```r
part_1_valid_passwords <- function(input) {
  is_valid <- function(i) {
    # extract the matching values from the string, we can then count the size of the vector to find how many times that
    # character appeared in the string
    lp <- length(str_extract_all(i$password, i$character)[[1]])
    # check to see if the password is valid
    i$min <= lp & lp <= i$max
  }
  
  input %>%
    split_input() %>%
    # run the is_valid function on each of the input values, keeping only the valid passwords
    keep(is_valid) %>%
    # pull out the password value from each of the lists
    map_chr("password")
}
part_1_valid_passwords(sample)
```

```
## [1] "abcde"     "ccccccccc"
```

This matches with the example given on the AOC website. We can now try to solve the first part:


```r
length(part_1_valid_passwords(actual))
```

```
## [1] 493
```

## Part 2

We can use the `split_input` function again, but modifiy the `is_valid` function slightly.


```r
part_2_valid_passwords <- function(input) {
  is_valid <- function(i) {
    # as per part 1, but this time extract the characters in the password at position min and max
    a <- str_sub(i$password, i$min, i$min) == i$character
    b <- str_sub(i$password, i$max, i$max) == i$character
    # we can now xor these values: if both a and b are true xor = FASLE
    xor(a, b)
  }
  
  input %>%
    split_input() %>%
    keep(is_valid) %>%
    map_chr("password")
}
part_2_valid_passwords(sample)
```

```
## [1] "abcde"
```

This matches the example, so we can now solve part 2.


```r
length(part_2_valid_passwords(actual))
```

```
## [1] 593
```
