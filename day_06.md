# Custom Customs



This is my attempt to solve [Day 6](https://adventofcode.com/2020/day/6).


```r
load_file <- function(file) {
  file %>%
    read_file() %>%
    # strip windows added carriage returns
    str_remove_all("\r") %>%
    # clear trailing whitespace
    str_trim() %>%
    # split where we have two new lines
    str_split("\n\n") %>%
    # take only the first results (the input is just one string)
    pluck(1) %>%
    # split each result into individual lines
    str_split("\n")
}

sample <- load_file("samples/day_06_sample.txt")
actual <- load_file("inputs/day_06_input.txt")
```

## Part 1

For part 1 I am going to first split each respondents response into the individual answers (`str_extract_all()`). This
will return a list of character vectors, so I will use `flatten_chr()` to turn these into vectors containing all of the
answers for each group as a character vector. Then I can simply take the `unique()` values and work out the `length()`
of the resulting vector. I tie all of this together with the `compose()` function from `{purrr}`: this is similar to
`%>%`, except it chains the functions together before you evaluate them into a single chain.

Like mathematical composition, `compose()` works with the "outer-most" function first, e.g. `str_extract_all()` runs
before `flatten_chr()`, etc. 


```r
part_1 <- function(input) {
  input %>%
    map_dbl(compose(length,
                    unique,
                    flatten_chr,
                    str_extract_all), ".") %>%
    sum()
}
```

Now I can check that this function behaves as expected:


```r
part_1(sample) == 11
```

```
## [1] TRUE
```

And we can run on our actual data


```r
part_1(actual)
```

```
## [1] 6273
```

## Part 2

For part 2 I am going to start as in part 1, split each respondents response into individual answers. But then I will
use `reduce()` to go through pairs of responses at a time and only select the questions which were answered by both as
"yes". We can then simply take the `length()` of each groups responses.


```r
part_2 <- function(input) {
  input %>%
    map(str_extract_all, ".") %>%
    map(reduce, ~.y[.y %in% .x]) %>%
    map_dbl(length) %>%
    sum()
}
```

We can test to see if the function works as expected::


```r
part_2(sample) == 6
```

```
## [1] TRUE
```

And run the function on the actual data.


```r
part_2(actual)
```

```
## [1] 3254
```

## Extra: solving part 1 & 2 with set functions

We could solve part 1 and 2 with the same function. Part 1 is essentially just the union of set's in each group, whereas
part 2 is the intersection of set's. So we can instead write a function that accepted a set function as an argument,
like so:


```r
extra <- function(input, fn) {
  input %>%
    map(str_extract_all, ".") %>%
    map(reduce, fn) %>%
    map_dbl(length) %>%
    sum()
}
```

We can now check that this new function works as expected:


```r
extra(actual, union) == part_1(actual)
```

```
## [1] TRUE
```

```r
extra(actual, intersect) == part_2(actual)
```

```
## [1] TRUE
```

---

*Elapsed Time: 0.551s*
