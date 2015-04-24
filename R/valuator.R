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

#' Least or Most Frequent Values
#'
#' \code{concat_rank_value_occurrence} takes a vector and returns the \code{n} least or most occuring values.
#'
#' @param x an atomic vector.
#' @param n a single positive integer indicating the number of values to return.
#' @param type a character string specifying which method should be used.
#' @return A character vector of length \code{n} that returns the least or most \code{n} occuring values.
#' @examples
#' x <- c(letters, letters[1:4])
#' frequent_values(x, n = 5L, type = "most")
frequent_values <- function(x, n = 5L, type = "most") {
  if(!is.atomic(x)) stop("x must be an atomic vector.")
  if(length(n) != 1L || !is.numeric(n) || any(n < 1)) stop("n must be a single positive integer.")
  if(!(type %in% c("most", "least"))) stop("type takes only two values: 'most' or 'least'")
  values <- sort(tapply(x, factor(x), length), decreasing = TRUE)
  if (type == "most"){
    values_processed <- concat_rank_value_occurrence(r = unname(head(rank(-values, ties.method = "min"), n)),
                                                     n = head(names(values), n),
                                                     o = head(unname(values), n))
  } else {
    values_processed <- concat_rank_value_occurrence(r = rev(tail(rank(values, ties.method = "min"), n)),
                                                     n = rev(tail(names(values), n)),
                                                     o = rev(tail(unname(values), n)))
  }
  return(paste(values_processed, collapse = " "))
}
