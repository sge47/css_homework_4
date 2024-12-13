HW04: Tidy data and Programming Exercises
================
Sophie Ennis

## Homework Description

This homework assignment has two parts: \* PART 1: practicing tidying
data with `tidyr` \* PART 2: practicing programming in R by applying the
programming techniques we have been learning so far

## PART 1

Load libraries

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   1.0.2
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.4
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.1
    ## ✔ readr   2.1.5     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rcis)
```

Load data

``` r
data("dadmom")
dadmom
```

    ## # A tibble: 3 × 5
    ##   famid named  incd namem  incm
    ##   <dbl> <chr> <dbl> <chr> <dbl>
    ## 1     1 Bill  30000 Bess  15000
    ## 2     2 Art   22000 Amy   18000
    ## 3     3 Paul  25000 Pat   50000

First, in a few sentences explain why the given data frame is not tidy
according to the tidy data principles we learned in the course, and
describe your approach to tidy it.

### Approach

The given data frame is not tidy because, according to the tidy data
principles, each variable needs to form a column and each observation
needs to form a row. The “inc-” and “name-” variables are split into two
columns to correspond with each type, dad or mom, when the income and
names of each person needs to make up one column each. The splitting of
these single variables into two columns each is thus not tidy, and we
need to create one column for the parents’ names and incomes. In order
to distinguish moms from dads, then, instead of adding an “-m” or “-d”
to the end of each column name, we need to create a new column called
“parent” that specifies if each observation is a mom or dad.

Next, write code to transform this data into a tidy format that adheres
to tidy data principles. Ensure your final output matches the structure
of the tidy data frame illustrated on the homework instructions page
(refer to HW04 on the website). Call this new dataframe `dadmom_tidy`

You can tidy the data using one or multiple piped operations (e.g., turn
the data longer, then wider, etc.). More than one approach can be taken
to solve this question and we tested several of them. Each approach is
valid as long as:

-   produces the correct output
-   you can explain it clearly
-   does not copy the same logic or specific combinations of `tidyr`
    functions suggested by AI-tools (while these solutions might not be
    wrong, we want you to try it out following your own logic for the
    assignment!)
-   uses `tidyr` functions or combines `tidyr` functions with `dplyr`
    functions

Tips: (1) Remember to check the documentation of each `tidyr` function
you use, it will help! (2) Consider using pen and paper to write your
logic to solve this before jumping into code, similarly to how we did in
class.

``` r
dadmom_names <- dadmom %>%
  select(famid, namem, named) %>%
  pivot_longer(
    cols = c(namem, named), 
    names_to = "parent", 
    values_to = "name"
  ) %>%
  mutate(parent = ifelse(parent == "namem", "m", "d")) 

print(dadmom_names)
```

    ## # A tibble: 6 × 3
    ##   famid parent name 
    ##   <dbl> <chr>  <chr>
    ## 1     1 m      Bess 
    ## 2     1 d      Bill 
    ## 3     2 m      Amy  
    ## 4     2 d      Art  
    ## 5     3 m      Pat  
    ## 6     3 d      Paul

``` r
dadmom_inc <- dadmom %>%
  select(famid, incm, incd) %>%
  pivot_longer(
    cols = c(incm, incd), 
    names_to = "parent", 
    values_to = "inc"
  ) %>%
  mutate(parent = ifelse(parent == "incm", "m", "d")) 

print(dadmom_inc)
```

    ## # A tibble: 6 × 3
    ##   famid parent   inc
    ##   <dbl> <chr>  <dbl>
    ## 1     1 m      15000
    ## 2     1 d      30000
    ## 3     2 m      18000
    ## 4     2 d      22000
    ## 5     3 m      50000
    ## 6     3 d      25000

``` r
dadmom_tidy <- dadmom_names %>%
  left_join(dadmom_inc, by = c("parent", "famid"))

print(dadmom_tidy)
```

    ## # A tibble: 6 × 4
    ##   famid parent name    inc
    ##   <dbl> <chr>  <chr> <dbl>
    ## 1     1 m      Bess  15000
    ## 2     1 d      Bill  30000
    ## 3     2 m      Amy   18000
    ## 4     2 d      Art   22000
    ## 5     3 m      Pat   50000
    ## 6     3 d      Paul  25000

After tidying the data, generate a plot using the exact code provided
below. Do not modify the code below: if you tidied the data frame
correctly in the previous step, the plot should generate without any
changes required. If errors occur, it indicates that the data was not
tidied correctly. This exercise demonstrates the efficiency of tidy data
in producing ggplots and performing data analysis tasks.

``` r
# DO NOT modify this code chunk: just run it. 
# if you tided the data correctly in the previous step, it should just work. 
# parent: m or d
# inc: individual income
# famid: family ID

ggplot(data = dadmom_tidy, mapping = aes(x = parent, y = inc)) +
  geom_point() +
  geom_line(mapping = aes(group = famid)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Gender parity and household income",
    subtitle = "Each line identifies a distinct family",
    x = "Mom or Dad",
    y = "Income",
  ) +
  theme_minimal()
```

![](programming_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## PART 2

This second part comprises five questions; please answer all of them.

### Question 1. Calculate the square of each element in the given vector `v` using a `for` loop and and a map function from the `purrr` package.

When using a for loop, provide two solutions: one that iterates directly
over the elements of vector `v` and one that iterates over the indexes
of `v`. In both cases, ensure you pre-allocate the output vector as
demonstrated in class. Write a few sentences highlighting the
differences between the two for loops and evaluating which approach
might be more effective in R, in which situation(s), and why. When using
a map function, choose the most suitable function. In a few sentences,
describe the advantages and limitations of using the map approach rather
than a for loop.

Use class materials to support your answers (readings, in-class code,
slides; a concise explanation of for loops is under Lecture 8 readings
on Base R Chapter 27).

``` r
v <- seq(from = 20, to = 1)
v
```

    ##  [1] 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1

Using a `for` loop looping over indexes

``` r
output_index <- vector(mode = "integer", length = length(v))
for (i in seq_along(v)) {  
  output_index[i] <- (v[i])^2
  print(output_index[i])
}
```

    ## [1] 400
    ## [1] 361
    ## [1] 324
    ## [1] 289
    ## [1] 256
    ## [1] 225
    ## [1] 196
    ## [1] 169
    ## [1] 144
    ## [1] 121
    ## [1] 100
    ## [1] 81
    ## [1] 64
    ## [1] 49
    ## [1] 36
    ## [1] 25
    ## [1] 16
    ## [1] 9
    ## [1] 4
    ## [1] 1

Using a `for` loop looping over elements

``` r
output_element <- vector(mode = "integer" , length = length(v))
for (i in v) {
  output_element <- i^2
  print(output_element)
}
```

    ## [1] 400
    ## [1] 361
    ## [1] 324
    ## [1] 289
    ## [1] 256
    ## [1] 225
    ## [1] 196
    ## [1] 169
    ## [1] 144
    ## [1] 121
    ## [1] 100
    ## [1] 81
    ## [1] 64
    ## [1] 49
    ## [1] 36
    ## [1] 25
    ## [1] 16
    ## [1] 9
    ## [1] 4
    ## [1] 1

To loop over the indices, I pre-allocated an empty vector with the same
length as v, iterated over each index i of v, and assigned values for
each element of v to the corresponding index. To loop over the elements,
I again pre-allocated an empty vector with the same length as v, but
this time I did not iterate over the indices of i and instead simply
assigned the output of each element to i. Looping over indices is
generally more effective in R in situations where we’re trying to do
more complicated things, like accessing values from multiple vectors,
because we’re able to loop over each position for each vector. For this
problem, looping over elements works just fine because it’s not very
complicated as we’re only squaring one value at a time in a simple
sequence.

Using a `map` function

``` r
output_map <- map_int(v, ~ .x ^ 2)
print(output_map)
```

    ##  [1] 400 361 324 289 256 225 196 169 144 121 100  81  64  49  36  25  16   9   4
    ## [20]   1

This map function takes a vector as input, applies a function to each
piece, and makes a double vector. The advantages of using a map function
like this is that it gets the job done much faster than a for loop. An
advantage of using map functions is that in just one line, we’ve
produced the same results as the previous, longer for loops. One
drawback of using map functions is that they vectorize over all
arguments so you cannot supply arguments that do not vary.

### Question 2. Calculate the square of all elements in the given matrix `m` using nested for loops and single for loops. Pre-allocate your output as shown in-class (here the output should be a matrix not a vector) and loop over the indexes of `m`. Write a few sentences to compare and contrast the two approaches (which was easier/harder for you to code, why, etc.).

``` r
m <- matrix(c(1:16), nrow = 4, ncol = 4, byrow = TRUE)
m
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## [3,]    9   10   11   12
    ## [4,]   13   14   15   16

Using nested `for` loops

``` r
output_nested <- matrix(numeric(length(m)), nrow = 4, ncol = 4)

for (i in 1:nrow(m)) {
  for (j in 1:ncol(m)) {
    output_nested[i, j] <- m[i, j]^2
  }
}
  print(output_nested)
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    4    9   16
    ## [2,]   25   36   49   64
    ## [3,]   81  100  121  144
    ## [4,]  169  196  225  256

Using one single `for` loop

``` r
output_single <- matrix(numeric(length(m)), nrow = 4, ncol = 4)

for (i in 1:length(m)) {
  output_single[i] <- (m[i])^2
}
print(output_single)
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    4    9   16
    ## [2,]   25   36   49   64
    ## [3,]   81  100  121  144
    ## [4,]  169  196  225  256

The nested for loops were more difficult for me to come up with because
I wasn’t sure what to come up with for the outside for loop. It took me
a while to realize that I could use \[i, j\] instead of splitting the
output into \[i\] and \[j\]. I split the for loops into accessing rows
and columns. The single for loop was easier to write because writing it
was more streamlined. It did take a bit of playing around with the
brackets to figure out how to display the output as a matrix.

### Question 3. Use the iris dataset to explore subsetting with `[]` and `[[ ]]` in R. The dataset contains 150 observations for three species (setosa, virginica, versicolor) of iris flower, and four features measured for each sample. All measurements are given in centimeters.

Load and explore the `iris` data, which is included in the `tidyverse`

``` r
data(iris)
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

Use `[]` to subset a variable of your choice (e.g., take a dataframe
column of your choice, and filter it based on a condition of your
choice). Save the result to a new object and check it’s data type using
one or more methods that we used in class or are in the readings.

``` r
long_petals <- iris["Petal.Length"] %>% 
  filter(Petal.Length > 1.5)

is.integer(long_petals)
```

    ## [1] FALSE

``` r
is.data.frame(long_petals)
```

    ## [1] TRUE

``` r
str(long_petals)
```

    ## 'data.frame':    113 obs. of  1 variable:
    ##  $ Petal.Length: num  1.7 1.6 1.7 1.7 1.7 1.9 1.6 1.6 1.6 1.6 ...

Use `[[]]` to do the same thing as above. Save the result to a new
object and check it’s data type using one or more methods that we used
in class or are in the readings.

``` r
wide_petals <- iris[["Petal.Width"]][iris[["Petal.Width"]] > 2.0]
wide_petals
```

    ##  [1] 2.5 2.1 2.2 2.1 2.5 2.1 2.4 2.3 2.2 2.3 2.3 2.1 2.1 2.2 2.3 2.4 2.1 2.4 2.3
    ## [20] 2.3 2.5 2.3 2.3

``` r
is.numeric(wide_petals)
```

    ## [1] TRUE

``` r
is.integer(wide_petals)
```

    ## [1] FALSE

``` r
is.double(wide_petals)
```

    ## [1] TRUE

``` r
str(wide_petals)
```

    ##  num [1:23] 2.5 2.1 2.2 2.1 2.5 2.1 2.4 2.3 2.2 2.3 ...

In a few sentences, explain why you get that datatype in each case;
then, explain when you would use `[[]]` vs. `[]` in a dataframe and why.

I got the data type of a dataframe when I used `[]` because one bracket
accesses subsets of the data while maintaining the structure of the
dataframe. I got the data type of a double when I used `[[]]` because
two brackets access individual elements as vectors. You would want to
use `[[]]` when you want to access a specific column as a vector or a
specific cell without the dataframe, because accessing the vector allows
for simplified operations. You would want to use `[]` when you’re
feeding data to functions that expect dataframes, like ggplot2 or dplyr,
because it keeps the output in dataframe form.

### Question 4. Compute the number of unique values in each column of the `iris` dataset. Write code that solves this task using the following ways: a `for` loop that iterates over indexes; one or more map functions; and `across()`. When using a `for` loop, make sure to pre-allocate your output as shown in-class. Refer to the readings and in-class materials (especially lecture 10) for examples.

Using a `for` loop

``` r
ncol(iris)
```

    ## [1] 5

``` r
nrow(iris)
```

    ## [1] 150

``` r
output_forloop <- data.frame(matrix(nrow = 1, ncol = ncol(iris)))
colnames(output_forloop) <- colnames(iris)
for (i in seq_along(iris)) {
  output_forloop[i] <- n_distinct(iris[[i]])
}
output_forloop
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           35          23           43          22       3

Using one or more `map` functions

``` r
library(purrr)
output_map <- map_df(iris, ~ n_distinct(.))
output_map
```

    ## # A tibble: 1 × 5
    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ##          <int>       <int>        <int>       <int>   <int>
    ## 1           35          23           43          22       3

Using `across`

``` r
output_across <- iris %>% 
  summarize(across(everything(), n_distinct))
output_across
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1           35          23           43          22       3

### Question 5. Create a new column to the iris dataset that categorizes each observation into three categories, based on the petal length feature.

If petal length is:

-   less than 2.0, label it as “small”
-   between 2.0 and 5.5 (inclusive), label it as “medium”
-   more than 5.5, label it as “big”

Then use `group_by` to group the data by petal length category, and
`summarize()` the average petal length for each category.

Answer this prompt in three different ways: the `ifelse()` or the
`if_else()` function; if / else statements; for loop. Then answer the
questions listed below.

Answer the prompt using the `ifelse()` or the `if_else()` function

``` r
iris %>% 
  rowwise() %>% 
  mutate(
    petal_length =
    ifelse(Petal.Length < 2.0, "small",
      ifelse(Petal.Length > 5.5, "big", "medium")
  )) %>% 
  ungroup() %>%
  group_by(petal_length) %>%
  summarize(
    avg_petal_length = mean(Petal.Length, na.rm = TRUE)
  )
```

    ## # A tibble: 3 × 2
    ##   petal_length avg_petal_length
    ##   <chr>                   <dbl>
    ## 1 big                      5.99
    ## 2 medium                   4.54
    ## 3 small                    1.46

Answer the prompt using if / else statements

``` r
iris %>% 
  rowwise() %>%
  mutate(
    petal_length = 
      if (Petal.Length < 2.0) {
        "small"}
    else if (Petal.Length >= 2.0 & Petal.Length <= 5.5) {
      "medium"}
   else if (Petal.Length > 5.5) {
     "big"} 
  ) %>% 
  ungroup() %>%
  group_by(petal_length) %>%
  summarize(
    avg_petal_length = mean(Petal.Length, na.rm = TRUE)
  )
```

    ## # A tibble: 3 × 2
    ##   petal_length avg_petal_length
    ##   <chr>                   <dbl>
    ## 1 big                      5.99
    ## 2 medium                   4.54
    ## 3 small                    1.46

Answer the prompt using a for loop (pre-allocate the output and loop
over indexes)

``` r
iris$petal_lengths <- NA_character_

for (i in seq_along(iris$petal_lengths)) {
  petal_lengths <- iris$Petal.Length[i]
  if (petal_lengths < 2.0) {
    iris$petal_lengths[i] <- "small"
  } else if (petal_lengths >= 2.0 & petal_lengths <= 5.5) {
    iris$petal_lengths[i] <- "medium"
  } else if (petal_lengths > 5.5) {
    iris$petal_lengths[i] <- "big"
  }} 
iris %>%
  group_by(petal_lengths) %>%
  summarize(
    avg_petal_lengths = mean(Petal.Length)
  ) 
```

    ## # A tibble: 3 × 2
    ##   petal_lengths avg_petal_lengths
    ##   <chr>                     <dbl>
    ## 1 big                        5.99
    ## 2 medium                     4.54
    ## 3 small                      1.46

In a few sentences, compare and contrast each method by answering the
following questions: Which method is the most suitable for this task,
and why? Which method did you find easier or harder to code? What are
the key differences between these approaches? Use class materials
(readings, in-class code, slides) to support your answers.

The most suitable method for answering this problem is ifelse() because
it is vectorized, meaning it applies the condition across all elements
in the column at once, making it the most efficient. I found the if/else
conditional statements to be the easiest to code because I think it’s
the most intuitive. Using a for loop with if/else conditional statements
was not as easy but not too hard to code because I had a similar example
to go off of from an in-class demonstration using the penguins dataset.
Using ifelse() was the least intuitive for me because after working out
the kinks, I needed to make an ifelse() statement inside another one and
only wrote an else for one of them. ifelse() is useful for assigning
values conditionally across an entire column or vector. A for loop is
not vectorized and applies operations element by element. if/else
conditional statements check each condition individually and are not
vectorized.

## Session info

``` r
# don't modify this code chunk
sessioninfo::session_info()
```

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.4.1 (2024-06-14)
    ##  os       Red Hat Enterprise Linux 8.10 (Ootpa)
    ##  system   x86_64, linux-gnu
    ##  ui       X11
    ##  language (EN)
    ##  collate  en_US.UTF-8
    ##  ctype    en_US.UTF-8
    ##  tz       America/Chicago
    ##  date     2024-11-07
    ##  pandoc   2.17.1.1 @ /usr/lib/rstudio-server/bin/quarto/bin/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package       * version date (UTC) lib source
    ##  assertthat      0.2.1   2019-03-21 [2] CRAN (R 4.2.1)
    ##  backports       1.4.1   2021-12-13 [2] CRAN (R 4.2.1)
    ##  broom           1.0.1   2022-08-29 [2] CRAN (R 4.2.1)
    ##  cellranger      1.1.0   2016-07-27 [2] CRAN (R 4.2.1)
    ##  cli             3.6.3   2024-06-21 [2] CRAN (R 4.4.1)
    ##  colorspace      2.0-3   2022-02-21 [2] CRAN (R 4.2.1)
    ##  crayon          1.5.2   2022-09-29 [2] CRAN (R 4.2.1)
    ##  DBI             1.1.3   2022-06-18 [2] CRAN (R 4.2.1)
    ##  dbplyr          2.2.1   2022-06-27 [2] CRAN (R 4.2.1)
    ##  digest          0.6.30  2022-10-18 [2] CRAN (R 4.2.1)
    ##  dplyr         * 1.1.4   2023-11-17 [2] CRAN (R 4.4.0)
    ##  evaluate        0.18    2022-11-07 [2] CRAN (R 4.2.1)
    ##  fansi           1.0.6   2023-12-08 [2] CRAN (R 4.4.0)
    ##  farver          2.1.1   2022-07-06 [2] CRAN (R 4.2.1)
    ##  fastmap         1.2.0   2024-05-15 [2] CRAN (R 4.4.0)
    ##  forcats       * 0.5.2   2022-08-19 [2] CRAN (R 4.2.1)
    ##  fs              1.5.2   2021-12-08 [2] CRAN (R 4.2.1)
    ##  gargle          1.2.1   2022-09-08 [2] CRAN (R 4.2.1)
    ##  generics        0.1.3   2022-07-05 [2] CRAN (R 4.2.1)
    ##  ggplot2       * 3.4.0   2022-11-04 [2] CRAN (R 4.2.1)
    ##  glue            1.7.0   2024-01-09 [2] CRAN (R 4.4.0)
    ##  googledrive     2.0.0   2021-07-08 [2] CRAN (R 4.2.1)
    ##  googlesheets4   1.0.1   2022-08-13 [2] CRAN (R 4.2.1)
    ##  gtable          0.3.1   2022-09-01 [2] CRAN (R 4.2.1)
    ##  haven           2.5.1   2022-08-22 [2] CRAN (R 4.2.1)
    ##  highr           0.11    2024-05-26 [2] CRAN (R 4.4.1)
    ##  hms             1.1.3   2023-03-21 [2] CRAN (R 4.3.0)
    ##  htmltools       0.5.8.1 2024-04-04 [2] CRAN (R 4.4.0)
    ##  httr            1.4.4   2022-08-17 [2] CRAN (R 4.2.1)
    ##  jsonlite        1.8.3   2022-10-21 [2] CRAN (R 4.2.1)
    ##  knitr           1.48    2024-07-07 [2] CRAN (R 4.4.1)
    ##  labeling        0.4.2   2020-10-20 [2] CRAN (R 4.2.1)
    ##  lifecycle       1.0.4   2023-11-07 [2] CRAN (R 4.4.0)
    ##  lubridate       1.9.0   2022-11-06 [2] CRAN (R 4.2.1)
    ##  magrittr        2.0.3   2022-03-30 [2] CRAN (R 4.2.1)
    ##  modelr          0.1.9   2022-08-19 [2] CRAN (R 4.2.1)
    ##  munsell         0.5.0   2018-06-12 [2] CRAN (R 4.2.1)
    ##  pillar          1.9.0   2023-03-22 [2] CRAN (R 4.3.0)
    ##  pkgconfig       2.0.3   2019-09-22 [2] CRAN (R 4.2.1)
    ##  purrr         * 1.0.2   2023-08-10 [2] CRAN (R 4.4.0)
    ##  R6              2.5.1   2021-08-19 [2] CRAN (R 4.2.1)
    ##  rcis          * 0.2.5   2024-08-20 [2] Github (css-materials/rcis@c0a0358)
    ##  readr         * 2.1.5   2024-01-10 [2] CRAN (R 4.4.0)
    ##  readxl          1.4.1   2022-08-17 [2] CRAN (R 4.2.1)
    ##  reprex          2.0.2   2022-08-17 [2] CRAN (R 4.2.1)
    ##  rlang           1.1.4   2024-06-04 [2] CRAN (R 4.4.1)
    ##  rmarkdown       2.28    2024-08-17 [2] CRAN (R 4.4.1)
    ##  rstudioapi      0.14    2022-08-22 [2] CRAN (R 4.2.1)
    ##  rvest           1.0.3   2022-08-19 [2] CRAN (R 4.2.1)
    ##  scales          1.2.1   2022-08-20 [2] CRAN (R 4.2.1)
    ##  sessioninfo     1.2.2   2021-12-06 [2] CRAN (R 4.2.1)
    ##  stringi         1.8.4   2024-05-06 [2] CRAN (R 4.4.0)
    ##  stringr       * 1.5.1   2023-11-14 [2] CRAN (R 4.4.0)
    ##  tibble        * 3.2.1   2023-03-20 [2] CRAN (R 4.3.0)
    ##  tidyr         * 1.3.0   2023-01-24 [2] CRAN (R 4.2.2)
    ##  tidyselect      1.2.1   2024-03-11 [2] CRAN (R 4.4.0)
    ##  tidyverse     * 1.3.2   2022-07-18 [2] CRAN (R 4.2.1)
    ##  timechange      0.1.1   2022-11-04 [2] CRAN (R 4.2.1)
    ##  tzdb            0.4.0   2023-05-12 [2] CRAN (R 4.3.0)
    ##  utf8            1.2.4   2023-10-22 [2] CRAN (R 4.4.0)
    ##  vctrs           0.6.5   2023-12-01 [2] CRAN (R 4.4.0)
    ##  withr           3.0.0   2024-01-16 [2] CRAN (R 4.4.0)
    ##  xfun            0.47    2024-08-17 [2] CRAN (R 4.4.1)
    ##  xml2            1.3.3   2021-11-30 [2] CRAN (R 4.2.1)
    ##  yaml            2.3.6   2022-10-18 [2] CRAN (R 4.2.1)
    ## 
    ##  [1] /home/sophieennis/R/x86_64-redhat-linux-gnu-library/4.4
    ##  [2] /usr/lib64/R/library
    ##  [3] /usr/share/R/library
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────

## Reflections & Resources

Write 1-2 paragraph reflecting on what was difficult and/or easy about
this homework assignment. Discuss the problems you encountered and how
you solved them, and new concepts or techniques you learned.

Please list the first and last names of any collaborators you worked
with to complete this assignment.

Additionally, list the resources you used and provide the links,
including how you utilized them (if you only used in-class resources,
just say so, you do not need to list them).

Kindly remember that you are not allowed to use AI tools to generate R
code for this and future assignments. The only acceptable uses of AI
tools are for debugging (but only after you have made an attempt on your
own) and for generating examples of how to use a specific function (but
also check the function documentation and course materials). If there
are different ways to code something, you should always prioritize what
learned in the course.

If you use AI, please explain how and be as specific as possible (e.g.,
“I used \[tool name\] to debug question 1”). Refer to the course website
for more on our AI and plagiarism policy. We conduct random checks on
submitted code, and while we trust this won’t be needed, any copied code
will result in penalties.

Tidying the dadmom dataset was the hardest part of this assignment
because we didn’t spend very long pn pivot_longer() or pivot_wider() and
the tidy principles, and they didn’t really stick in my mind. It
honestly took me a long time. Unlike Part 1, Part 2 went far more
smoothly though it contained more questions. I understand for loops and
the underlying reasons why different numbers of brackets access elements
vs indices, so Questions 1, 2, 3, and 4 in Part 2 were more intuitive.
Using `[[]]` in Question 3 was harder than using `[]` because it took me
awhile to figure out that I needed to repeat iris\[\[“Petal.Width”\]\]
but make it \> 2.0 within a third bracket. I feel the most comfortable
with if/else conditional statements so for Question 5, Rosa and I
started there and based the ifelse() version and the for loop version on
what we figured out for the conditional statements. Question 5 largely
reflected the in-class demonstration we did with the penguins dataset,
which I found extremely helpful because I’ve found most of the
assignments to not reflect what we’re doing in class as much.

-   I worked with Rosa Lander on the code for this assignment.
-   I primarily used the slides and in-class demonstrations to complete
    this assignment.
-   I used the [tidyr
    documentation](https://tidyr.tidyverse.org/reference/pivot_longer.html)
    for help on tidying the dadmom dataset with pivot_longer() in Part
    1.
-   I used the [purrr
    documentation](https://purrr.tidyverse.org/reference/map.html) for
    help on map functions in Part 2 Questions 1 and 4.
-   I used ChatGPT to debug the map function in Question 4 because I
    couldn’t figure out why ndistinct() wasn’t working and it was
    because it was missing a period in the parentheses.
