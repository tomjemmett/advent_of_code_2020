# Passport Processing



This is my attempt to solve [Day 4](https://adventofcode.com/2020/day/4).


```r
sample <- read_lines("samples/day_04_sample.txt")
actual <- read_lines("inputs/day_04_input.txt")
```

## Part 1

The fields for todays puzzle are:

 - byr (Birth Year)
 - iyr (Issue Year)
 - eyr (Expiration Year)
 - hgt (Height)
 - hcl (Hair Color)
 - ecl (Eye Color)
 - pid (Passport ID)
 - cid (Country ID)

For a record to be treated as valid we must have all of the fields present, except for the country id which is optional.

Records are separated by blank new lines. I am going to first collapse the data into a single string, separated by ",".
The blank lines will then be ",,", so we can simply split our input there. This will give us one string per records. We
then need to sort out each record by splitting each string at either a space or a comma, then using the `unglue_data`
function to extra a data frame with a column "key" for the left hand part and a column "value" for the right hand part.

By using `map_dfr` we will be able to combine each record's key/value pairs into a single dataframe, but we add a
column "record" to keep track of which record we are dealing with.


```r
process_data <- function(input) {
  input %>%
    paste(collapse = ",") %>%
    str_split(",,") %>%
    # take just the first result returned by str_split, it will return a list
    # with one item which contains the results
    pluck(1) %>%
    str_split("[, ]") %>%
    map_dfr(unglue::unglue_data, "{key}:{value}", .id = "record")
}
process_data(sample)
```

```
##    record key     value
## 1       1 ecl       gry
## 2       1 pid 860033327
## 3       1 eyr      2020
## 4       1 hcl   #fffffd
## 5       1 byr      1937
## 6       1 iyr      2017
## 7       1 cid       147
## 8       1 hgt     183cm
## 9       2 iyr      2013
## 10      2 ecl       amb
## 11      2 cid       350
## 12      2 eyr      2023
## 13      2 pid 028048884
## 14      2 hcl   #cfa07d
## 15      2 byr      1929
## 16      3 hcl   #ae17e1
## 17      3 iyr      2013
## 18      3 eyr      2024
## 19      3 ecl       brn
## 20      3 pid 760753108
## 21      3 byr      1931
## 22      3 hgt     179cm
## 23      4 hcl   #cfa07d
## 24      4 eyr      2025
## 25      4 pid 166559648
## 26      4 iyr      2011
## 27      4 ecl       brn
## 28      4 hgt      59in
```


```r
get_valid_records <- function(input) {
  input %>%
    process_data() %>%
    filter(key != "cid") %>%
    group_by(record) %>%
    filter(n() == 7) %>%
    ungroup()
}
```


```r
part_1 <- function(input) {
  input %>%
    get_valid_records() %>%
    distinct(record) %>%
    nrow()
}
```

We can test our function on the sample:


```r
part_1(sample) == 2
```

```
## [1] TRUE
```

Now we can run our function on the actual data:


```r
part_1(actual)
```

```
## [1] 233
```

## Part 2

We now need to validate the data in the passports:

 - byr (Birth Year) - four digits; at least 1920 and at most 2002.
 - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
 - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
 - hgt (Height) - a number followed by either cm or in:
   - If cm, the number must be at least 150 and at most 193.
   - If in, the number must be at least 59 and at most 76.
 - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
 - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
 - pid (Passport ID) - a nine-digit number, including leading zeroes.
 - cid (Country ID) - ignored, missing or not.

The approach I am going to take for part 2 is to build some helper validation functions for the years and the height
parts of the record, and then `filter` the rows to just retain valid records. To make it easier to do this I will first
pivot the data from long format to wide, so each passport is one row of data.


```r
part_2 <- function(input) {
  validate_years <- function(y, min, max) {
    yi <- suppressWarnings(as.integer(y))
    ifelse(is.na(yi), FALSE, min <= yi & yi <= max)
  }
  
  validate_height <- function(h) {
    hv <- suppressWarnings(as.integer(str_sub(h, 1, -3)))
    ht <- str_sub(h, -2, -1)
    case_when(is.na(hv) ~ FALSE,
              ht == "cm" ~ 150 <= hv & hv <= 193,
              ht == "in" ~  59 <= hv & hv <=  76,
              TRUE ~ FALSE)
  }
  
  input %>%
    get_valid_records() %>%
    pivot_wider(names_from = key, values_from = value) %>%
    filter(validate_years(byr, 1920, 2002),
           validate_years(iyr, 2010, 2020),
           validate_years(eyr, 2020, 2030),
           validate_height(hgt),
           str_detect(hcl, "^#[0-9a-f]{6}$"),
           ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
           str_detect(pid, "^\\d{9}$"))
}
```

The provided test cases don't use the initial sample data, so let's just run the function and see if it does not error.


```r
part_2(sample)
```

```
## [90m# A tibble: 2 x 8[39m
##   record ecl   pid       eyr   hcl     byr   iyr   hgt  
##   [3m[90m<chr>[39m[23m  [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m     [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m   [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m
## [90m1[39m 1      gry   860033327 2020  #fffffd 1937  2017  183cm
## [90m2[39m 3      brn   760753108 2024  #ae17e1 1931  2013  179cm
```

It seems to work, so let's run on our actual data


```r
nrow(part_2(actual))
```

```
## [1] 111
```
