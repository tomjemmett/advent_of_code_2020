--- 
title: "Advent of Code 2020"
author: "[Tom Jemmett](mailto:thomas.jemmett@nhs.net)"
date: "2020-12-09"
site: bookdown::bookdown_site
output:
  bookdown::gitbook:
    df_print: "kable"
    config:
      toc:
        collapse: section
        before: |
          <li><strong><a href="./">Advent of Code 2020</a></strong></li>
documentclass: book
link-citations: yes
github-repo: tomjemmett/advent-of-code-2020
description: "My attempt to complete AOC 2020 in R"
---

# Introduction {-}

This is my attempt to complete the [Advent of Code 2020](https://adventofcode.com/2020/) in R using RMarkdown, along
with bookdown to render the site. The book will automatically be rebuilt using
[GitHub actions](https://github.com/features/actions) and hosted using [GitHub Pages](https://pages.github.com/).
