# Monster Messages



This is my attempt to solve [Day 19](https://adventofcode.com/2020/day/19).

This is a work in progress: part 1 works, part 2 gives an incorrect result.


```r
process_file <- function(file) {
  read_file(file) %>%
    str_trim() %>%
    str_remove_all("\r") %>%
    str_split("\n\n") %>%
    pluck(1) %>%
    map(str_split, "\n") %>%
    map(1)
}
sample <- process_file("samples/day_19_sample.txt")
actual <- process_file("inputs/day_19_input.txt")
```

## Part 1

First let's create a function to process a rule against the current message. `r` is the rule we are currently processing
and defaults to "0", `m` is the character index in message that we are currently processing.

If we do not match the message we return `NA`, othwerwise we return the postion that we have reached in the message.


```r
fn <- function(rules, message, r = "0", m = 1) {
  if (m > length(message)) return (as.numeric(NA))
  
  if (str_detect(rules[[r]], "^\\w$")) {
    return (if (rules[[r]] == message[[m]]) m + 1 else as.numeric(NA))
  }
  
  for (i in str_split(rules[[r]], " \\| ")[[1]]) {
    mx <- m
    for (j in str_split(i, " ")[[1]]) {
      mx <- fn(rules, message, j, mx)
      if (is.na(mx)) break()
    }
    if (!is.na(mx)) return (mx)
  }
  return (as.numeric(NA))
}
```

Now we just need to process our input to split into rules and messages, and then call our function.


```r
part_1 <- function(input) {
  rules <- local({
    a <- input[[1]] %>%
      str_replace_all("\"", "") %>%
      str_split(": ") %>%
      transpose()
    set_names(a[[2]], a[[1]])
  })
  
  messages <- input[[2]] %>%
    str_extract_all(".")
  
  m <- map_dbl(messages, fn, rules = rules)
  
  sum(map_dbl(messages, length) == m - 1, na.rm = TRUE)
}
```

We can test our function matches the sample:


```r
part_1(sample) == 2
```

```
## [1] TRUE
```

And we can run with our actual data:


```r
part_1(actual)
```

```
## [1] 173
```

## Part 2

This is a work in progress, it's not giving the correct result.


```r
part_2 <- function(input) {
  rules <- local({
    a <- input[[1]] %>%
      str_replace_all("\"", "") %>%
      str_split(": ") %>%
      transpose()
    set_names(a[[2]], a[[1]])
  })
  
  rules[["8"]] <- "42 | 42 8"
  rules[["11"]] <- "42 31 | 42 11 31"
  
  messages <- input[[2]] %>%
    str_extract_all(".")
  
  m <- map_dbl(messages, fn, rules = rules)
  
  sum(map_dbl(messages, length) == m - 1, na.rm = TRUE)
}
```


```r
part_2_sample <- list(c(
  "42: 9 14 | 10 1",
  "9: 14 27 | 1 26",
  "10: 23 14 | 28 1",
  "1: \"a\"",
  "11: 42 31",
  "5: 1 14 | 15 1",
  "19: 14 1 | 14 14",
  "12: 24 14 | 19 1",
  "16: 15 1 | 14 14",
  "31: 14 17 | 1 13",
  "6: 14 14 | 1 14",
  "2: 1 24 | 14 4",
  "0: 8 11",
  "13: 14 3 | 1 12",
  "15: 1 | 14",
  "17: 14 2 | 1 7",
  "23: 25 1 | 22 14",
  "28: 16 1",
  "4: 1 1",
  "20: 14 14 | 1 15",
  "3: 5 14 | 16 1",
  "27: 1 6 | 14 18",
  "14: \"b\"",
  "21: 14 1 | 1 14",
  "25: 1 1 | 1 14",
  "22: 14 14",
  "8: 42",
  "26: 14 22 | 1 20",
  "18: 15 15",
  "7: 14 5 | 1 21",
  "24: 14 1"
), c(
  "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
  "bbabbbbaabaabba",
  "babbbbaabbbbbabbbbbbaabaaabaaa",
  "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
  "bbbbbbbaaaabbbbaaabbabaaa",
  "bbbababbbbaaaaaaaabbababaaababaabab",
  "ababaaaaaabaaab",
  "ababaaaaabbbaba",
  "baabbaaaabbaaaababbaababb",
  "abbbbabbbbaaaababbbbbbaaaababb",
  "aaaaabbaabaaaaababaa",
  "aaaabbaaaabbaaa",
  "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
  "babaaabbbaaabaababbaabababaaab",
  "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
))
```


```r
part_1(part_2_sample) == 3
```

```
## [1] TRUE
```

```r
part_2(part_2_sample)
```

```
## [1] 6
```


```r
part_2(actual)
```

```
## [1] 221
```
