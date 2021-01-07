# Ticket Translation



This is my attempt to solve [Day 16](https://adventofcode.com/2020/day/16).


```r
# process the files
process_file <- function(file) {
  file %>%
    # strip windows new line carriage return charaters
    str_replace_all("\r", "") %>%
    # remove trailing whitespace
    str_trim() %>%
    # split on double new lines
    str_split("\n\n") %>%
    # get the first result (input was a single vector)
    pluck(1) %>%
    # iterate over these results, split on newlines
    map(str_split, "\n") %>%
    # iterate over the results, select just the first item from each
    # (each result from str_split will be a list of character vectors)
    map(1)
}
sample <- read_file("samples/day_16_sample.txt") %>% process_file()
actual <- read_file("inputs/day_16_input.txt") %>% process_file()
```

## Part 1

First let's build a function to process our "notes". We can return a list containing the fields we are expecting (as a
dataframe with columns for the ranges), our ticket, and the nearby tickets.


```r
process_notes <- function(input) {
  fields <- input[[1]] %>%
    unglue_data("{field}: {a}-{b} or {c}-{d}", convert = TRUE)
  
  my_ticket <- as.numeric(str_split(input[[2]][[2]], ",")[[1]])
  nearby_tickets <- map(str_split(input[[3]][-1], ","), as.numeric)
  
  list(fields = fields,
       my_ticket = my_ticket,
       nearby_tickets = nearby_tickets)
}
```

We can now process our `sample` and `actual` data.


```r
psample <- process_notes(sample)
pactual <- process_notes(actual)

psample
```

```
## $fields
##   field  a  b  c  d
## 1 class  1  3  5  7
## 2   row  6 11 33 44
## 3  seat 13 40 45 50
## 
## $my_ticket
## [1]  7  1 14
## 
## $nearby_tickets
## $nearby_tickets[[1]]
## [1]  7  3 47
## 
## $nearby_tickets[[2]]
## [1] 40  4 50
## 
## $nearby_tickets[[3]]
## [1] 55  2 20
## 
## $nearby_tickets[[4]]
## [1] 38  6 12
```

Now, we need to check if a value is valid. We can build a simple function to do this that will take a value and the
fields dataset. It will return `TRUE` if the value is valid for one of the fields and `FALSE` otherwise.


```r
value_is_valid <- function(value, fields) {
  any(
    fields$a <= value & value <= fields$b,
    fields$c <= value & value <= fields$d
  )
}
```

We now can take the `nearby_tickets`, flatten the nested list to a single numeric vector and discard any value that is
not valid.


```r
part_1 <- function(input) {
  input$nearby_tickets %>%
    flatten_dbl() %>%
    discard(value_is_valid, fields = input$fields) %>%
    sum()
}
```

We can test with our sample data:


```r
part_1(psample) == 71
```

```
## [1] TRUE
```

And we can run with our actual data:


```r
part_1(pactual)
```

```
## [1] 21978
```

## Part 2

We have a different sample for part 2:


```r
psample_2 <- str_trim("
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
") %>%
  process_file() %>%
  process_notes()
```

For part 2 we can use `{dplyr}` to find the possible fields that a value could take at a position, then we can loop
over the possible fields reducing the rows in the table by finding a field that is the only candidate for a position,
then removing that field from other positions.

This assumes that there will always be just 1 possible candidate at each point.


```r
part_2 <- function(input) {
  valid_tickets <- input$nearby_tickets %>%
    keep(~all(map_lgl(.x, value_is_valid, fields = input$fields)))
  
  possible_fields <- data.frame(value = flatten_dbl(valid_tickets),
                                pos = 1:nrow(input$fields)) %>%
    mutate(fields = map(value, function(.x) {
      input$fields %>%
        filter((a <= .x & .x <=b) | (c <= .x & .x <= d)) %>%
        pull(field)
    })) %>%
    unnest_longer(col = fields) %>%
    group_by(pos) %>%
    count(fields) %>%
    filter(n == max(n))
  
  input$fields$pos <- 0
  
  while (nrow(possible_fields) > 0) {
    f <- filter(possible_fields, n() == 1)
    possible_fields <- filter(possible_fields, fields != f$fields)
    
    input$fields[input$fields$field == f$fields, "pos"] <- f$pos
  }

  input$fields %>%
    select(field, pos)
}
```

We can test our function matches the provided sample:


```r
part_2(psample_2)
```

```
##   field pos
## 1 class   2
## 2   row   1
## 3  seat   3
```

And now we can solve the actual problem. Find the fields and their positions, the apply those positions to `my_ticket`:


```r
ps <- pactual %>%
  part_2() %>%
  filter(str_detect(field, "^departure")) %>%
  pull(pos)

prod(pactual$my_ticket[ps])
```

```
## [1] 1053686852011
```

---

*Elapsed Time: 8.078s*
