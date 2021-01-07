# Handheld Halting



This is my attempt to solve [Day 8](https://adventofcode.com/2020/day/8).


```r
sample <- read_lines("samples/day_08_sample.txt")
actual <- read_lines("inputs/day_08_input.txt")
```

## Part 1

Today I'm going to build an [R6 class](https://adv-r.hadley.nz/r6.html) to handle the state of the computer.


```r
Computer <- R6Class(
  "Computer",
  public = list(
    initialize = function(instructions) {
      private$instructions <- instructions %>%
        unglue_data("{inst} {i}", convert = TRUE) %>%
        mutate(c = 0)
    },
    get_accumulator = function() {
      private$accumulator
    },
    get_instructions_count = function() {
      sum(private$instructions$c)
    },
    run_next = function() {
      next_instruction <- private$next_instruction()
      private$run_instruction(next_instruction$inst,
                              next_instruction$i)
      
      invisible(self)
    },
    run_until_repeat = function() {
      repeat {
        next_instruction <- private$next_instruction()
        if (next_instruction$c > 0) {
          break()
        }
        private$run_instruction(next_instruction$inst,
                                next_instruction$i)
      }
      
      invisible(self)
    },
    print = function(...) {
      i <- private$instructions[private$ptr, ]
      cat("Accumulator:     ", private$accumulator, "\n")
      cat("Pointer:         ", private$ptr, "\n")
      cat("Instructions Ran:", self$get_instructions_count(), "\n")
      cat("Next Instruction:",
          i$inst,
          " ",
          sprintf("%+d", i$i),
          " (",
          i$c,
          ")\n")
    }
  ),
  private = list(
    accumulator = 0,
    ptr = 1,
    instructions = list(),
    
    next_instruction = function() {
      private$instructions[private$ptr, ]
    },
    run_instruction = function(inst, i) {
      private$instructions[private$ptr, "c"] <- 1 +
        private$instructions[private$ptr, "c"]
      
      private$ptr <- private[[inst]](i)
    },
    # instruction functions: must return the next pointer value
    acc = function(i) {
      private$accumulator <- private$accumulator + i
      private$ptr + 1
    },
    jmp = function(i) {
      private$ptr + i
    },
    nop = function(i) {
      private$ptr + 1
    }
  )
)
```

Now we can initialize our sample computer:


```r
sample_computer <- Computer$new(sample)
sample_computer
```

```
## Accumulator:      0 
## Pointer:          1 
## Instructions Ran: 0 
## Next Instruction: nop   +0  ( 0 )
```

And we can run the next instruction:


```r
sample_computer$run_next()
sample_computer
```

```
## Accumulator:      0 
## Pointer:          2 
## Instructions Ran: 1 
## Next Instruction: acc   +1  ( 0 )
```

We can run the computer until we repeat an instruction:


```r
sample_computer$run_until_repeat()
sample_computer
```

```
## Accumulator:      5 
## Pointer:          2 
## Instructions Ran: 7 
## Next Instruction: acc   +1  ( 1 )
```

According to the days text, the accumulator should be 5, and we should have run 7 instructions, which we can see is the
output from the `sample_computer`.

Now we can run the actual data:


```r
actual_computer <- Computer$new(actual)
actual_computer$run_until_repeat()
actual_computer
```

```
## Accumulator:      1816 
## Pointer:          580 
## Instructions Ran: 211 
## Next Instruction: jmp   -101  ( 1 )
```

## Part 2

We need to modify our computer slightly. First, we need to introduce a way to see if our computer has
[halted](https://en.wikipedia.org/wiki/Halting_problem), which will be when the pointer has exceeded the length of the
instructions.

Second, we need to be able to flip `nop` to `jmp` and `jmp` to `nop`. There may be a smarter way to figure out which to
flip, but I'm just going to iterate through the initial input and flip one at a time. If the computer halt's then we
have found our solution.

Rather than recreating the `Computer` class, we can use `Computer$set()` to add a `halted` method and a 
`run_until_halted_or_repeat` method.


```r
Computer$set("public", "is_halted", function() {
  private$ptr > nrow(private$instructions)
})

Computer$set("public", "run_until_halted_or_repeat", function() {
  repeat {
    if (self$is_halted()) {
      break()
    }
    next_instruction <- private$next_instruction()
    if (next_instruction$c > 0) {
      break()
    }
    private$run_instruction(next_instruction$inst,
                            next_instruction$i)
  }
  
  invisible(self)
})
```

Now we need to build a function to take our list of instructions and flip all of the `nop`/`jmp` instructions:


```r
flip_instructions <- function(input) {
  input %>%
    str_detect("^(?!acc)") %>%
    which() %>%
    map(function(.x) {
      ix <- input[[.x]]
      str_sub(input[[.x]], 1, 3) <- if(str_sub(input[[.x]], 1, 3) == "nop") {
        "jmp"
      } else {
        "nop"
      }
      input
    })
}
```

Our sample flipped looks like this:


```r
flip_instructions(sample)
```

```
## [[1]]
## [1] "jmp +0"  "acc +1"  "jmp +4"  "acc +3"  "jmp -3"  "acc -99" "acc +1" 
## [8] "jmp -4"  "acc +6" 
## 
## [[2]]
## [1] "nop +0"  "acc +1"  "nop +4"  "acc +3"  "jmp -3"  "acc -99" "acc +1" 
## [8] "jmp -4"  "acc +6" 
## 
## [[3]]
## [1] "nop +0"  "acc +1"  "jmp +4"  "acc +3"  "nop -3"  "acc -99" "acc +1" 
## [8] "jmp -4"  "acc +6" 
## 
## [[4]]
## [1] "nop +0"  "acc +1"  "jmp +4"  "acc +3"  "jmp -3"  "acc -99" "acc +1" 
## [8] "nop -4"  "acc +6"
```

We have 4 set's of instructions to test. This isn't the most efficient way of solving this... we could flip each of the
instructions in turn, and run the computer on that. This would allow us to exit early. But I don't think that this will
cause us much issues as our actual input isn't huge (`length(actual)` instructions).

Now we just need to build a function to iterate over the flipped instructions and run until we find a solution, and
return the results of that computer.


```r
part_2 <- function(input) {
  for (i in flip_instructions(input)) {
    computer <- Computer$new(i)
    computer$run_until_halted_or_repeat()
    
    if (computer$is_halted()) {
      return(list(input = input, computer = computer))
    }
  }
  stop("No solution found!")
}
```


```r
part_2(sample)
```

```
## $input
## [1] "nop +0"  "acc +1"  "jmp +4"  "acc +3"  "jmp -3"  "acc -99" "acc +1" 
## [8] "jmp -4"  "acc +6" 
## 
## $computer
## Accumulator:      8 
## Pointer:          10 
## Instructions Ran: 6 
## Next Instruction: NA   NA  ( NA )
```

This is the result that we are expecting, so we can run this for the actual data:


```r
part_2(actual)$computer
```

```
## Accumulator:      1149 
## Pointer:          627 
## Instructions Ran: 123 
## Next Instruction: NA   NA  ( NA )
```

## Extra: update the computer class

Redefining the entire class to include the added methods in part 2, and to redefine the print method


```r
Computer <- R6Class(
  "Computer",
  public = list(
    initialize = function(instructions) {
      private$instructions <- instructions %>%
        unglue_data("{inst} {i}", convert = TRUE) %>%
        mutate(c = 0)
    },
    get_accumulator = function() {
      private$accumulator
    },
    get_instructions_count = function() {
      sum(private$instructions$c)
    },
    run_next = function() {
      next_instruction <- private$next_instruction()
      private$run_instruction(next_instruction$inst,
                              next_instruction$i)
      
      invisible(self)
    },
    run_until_repeat = function() {
      repeat {
        next_instruction <- private$next_instruction()
        if (next_instruction$c > 0) {
          break()
        }
        private$run_instruction(next_instruction$inst,
                                next_instruction$i)
      }
      
      invisible(self)
    },
    is_halted = function() {
      private$ptr > nrow(private$instructions)
    },
    run_until_halted_or_repeat = function() {
      repeat {
        if (self$is_halted()) {
          break()
        }
        next_instruction <- private$next_instruction()
        if (next_instruction$c > 0) {
          break()
        }
        private$run_instruction(next_instruction$inst,
                                next_instruction$i)
      }
      
      invisible(self)
    },
    print = function(...) {
      cat("Accumulator:     ", private$accumulator, "\n")
      cat("Pointer:         ", private$ptr, "\n")
      cat("Instructions Ran:", self$get_instructions_count(), "\n")
      cat("Next Instruction: ")
      if (self$is_halted()) {
        cat("HALTED\n")
      } else {
        i <- private$instructions[private$ptr, ]
        cat(i$inst,
            " ",
            sprintf("%+d", i$i),
            " (",
            i$c,
            ")\n")
      }
    }
  ),
  private = list(
    accumulator = 0,
    ptr = 1,
    instructions = list(),
    
    next_instruction = function() {
      private$instructions[private$ptr, ]
    },
    run_instruction = function(inst, i) {
      private$instructions[private$ptr, "c"] <- 1 +
        private$instructions[private$ptr, "c"]
      
      private$ptr <- private[[inst]](i)
    },
    # instruction functions: must return the next pointer value
    acc = function(i) {
      private$accumulator <- private$accumulator + i
      private$ptr + 1
    },
    jmp = function(i) {
      private$ptr + i
    },
    nop = function(i) {
      private$ptr + 1
    }
  )
)
```

---

*Elapsed Time: 35.045s*
