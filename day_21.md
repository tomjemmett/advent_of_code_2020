# Allergen Assessment



This is my attempt to solve [Day 21](https://adventofcode.com/2021/day/21).


```r
sample <- read_lines("samples/day_21_sample.txt")
actual <- read_lines("inputs/day_21_input.txt")
```

# Part 1

First, let's build a function which takes the input and return's all of the possible combinations (the "cross product")
of ingredients and allergens.


```r
ingredient_allergen_combinations <- function(input) {
  input %>%
    str_remove_all("contains |\\)") %>%
    str_split(" \\(") %>%
    map_dfr(~list(
      ingredient = str_split(.x[[1]], " ")[[1]],
      allergen = str_split(.x[[2]], ", ")[[1]]
    ) %>% cross_df(), .id = "food")
}
```

Next we need to find for each allergen which ingredients could be that allergen. We can do this by simply counting the
allergen's and ingredients, then for each allergen filter out all of the ingredients which don't appear in every food.

Here we do this by grouping by allergen's and then filtering the `n` column (created by `count()`) to only include rows
which are equal to the maximum value.


```r
get_allergen_ingredients <- function(data) {
  data %>%
    group_by(allergen) %>%
    count(ingredient) %>%
    filter(n == max(n)) %>%
    ungroup()
}
```

We can now put this together to solve part 1. We simply need to take the data frame containing all of the foods and
their ingredients, find the possibly ingredients for each allergen, and then remove these from the data frame.


```r
part_1 <- function(input) {
  df <- ingredient_allergen_combinations(input)
  
  allergen_ingredients <- df %>%
    get_allergen_ingredients() %>%
    distinct(ingredient)
  
  df %>%
    anti_join(allergen_ingredients, by = "ingredient") %>%
    distinct(food, ingredient) %>%
    nrow()
}
```

We can test our function works against the sample:


```r
part_1(sample) == 5
```

```
## [1] TRUE
```

And we can run our function on the actual data:


```r
part_1(actual)
```

```
## [1] 2307
```

# Part 2

For part 2 we can reuse the functions we created in part 1 and use the `{igraph}` package to create a bipartite graph
of the ingredients and allergen's, then find the maximum bipartite match for this graph. This will give us our answer.


```r
part_2 <- function(input) {
  df <- ingredient_allergen_combinations(input)
  
  allergen_ingredients <- df %>%
    get_allergen_ingredients()
  
  vertices <- bind_rows(
    tibble(name = unique(allergen_ingredients$ingredient), type = 1),
    tibble(name = unique(allergen_ingredients$allergen), type = 0)
  )
  
  g <- allergen_ingredients %>%
    select(-n) %>%
    graph_from_data_frame(vertices = vertices)
  
  m <- max_bipartite_match(g)$matching
  
  paste(m[sort(filter(vertices, type == 0)$name)], collapse = ",")
}
```

We can test that our function works on the provided sample data:


```r
part_2(sample) == "mxmxvkd,sqjhc,fvjkl"
```

```
## [1] TRUE
```

And we can run our function on the actual data:


```r
part_2(actual)
```

```
## [1] "cljf,frtfg,vvfjj,qmrps,hvnkk,qnvx,cpxmpc,qsjszn"
```
