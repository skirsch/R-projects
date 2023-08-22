# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
install.packages("DataEditR")
library(DataEditR)

select_loc <- function(data, ...) {
  eval_select(rlang::expr(c(...)), data)
}

rename_loc <- function(data, ...) {
  eval_rename(rlang::expr(c(...)), data)
}

# a, means we can operate on vectors, etc.
steve <- function(a) {
    if (a>3)
      a+3
    else
      a-1
}
steve1 <- function(x){
  apply(x,c(1,2),steve)
  # this works on any size dataframe
}

# can now do steve1(cars) which returns a matrix
# good, good intervention, bad, bad intervention
ft <- function(a,b,c,d) {
  fisher.test(matrix(c(a-c,c,b-d,d),2,2))
}
