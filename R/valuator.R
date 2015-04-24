#' valuator: A package for valuing customers.
#'
#' The foo package provides three categories of important functions:
#' foo, bar and baz.
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name foo
NULL

#' Calculate Lifetime Value - Simplified
#'
#' \code{concat_rank_value_occurrence} takes vectors representing rank, value, and number of occurrences and returns them in a concatenated string.
#'
#' @param gc an atomic vector representing the rank of the values.
#' @param r an atomic vector representing the value itself.
#' @param d an atomic vector representing the number of occurrences of that value.
#' @return  A character vector where each element is the concatenation of rank, value, and number of occurrences.
#' @examples
#' # Let's make a comment
#' concat_rank_value_occurrence("value rank", "value", "number of value occurrences")
#' concat_rank_value_occurrence(1, "green", "15")
#' concat_rank_value_occurrence(5, 6, 7)
calculate_lifetime_value <- function(gc, r, d) {
  return(gc * (r / (1 + d + r)))
}
