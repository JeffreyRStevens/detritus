
#' Test that vector has a maximum number of elements
#'
#' @param x Vector
#' @param num Numeric scalar of maximum number of elements
#'
#' @return
#' Error if maximum number is exceeded; otherwise, nothing.
#'
#' @export
#'
#' @examples
#' check_element_max(1:5, 5)
#' \dontrun{
#' check_element_max(1:5, 4)
#' }
check_element_max <- function(x, num) {
  if(length(x) > num) {
    cli::cli_abort(paste0(
      "`{substitute(x)}` must have no more than {num} element{?s}."
    ))
  }
}

#' Test that vector has a minimum number of elements
#'
#' @param x Vector
#' @param num Numeric scalar of minimum number of elements
#'
#' @return
#' Error if minimum number is not met; otherwise, nothing.
#'
#' @export
#'
#' @examples
#' check_element_min(1:5, 5)
#' \dontrun{
#' check_element_min(1:5, 6)
#' }
check_element_min <- function(x, num) {
  if(length(x) < num) {
    cli::cli_abort(paste0(
      "`{substitute(x)}` must have at least {num} element{?s}."
    ))
  }
}

#' Test that vector has exact number of elements
#'
#' @param x Vector
#' @param num Numeric scalar of number of elements
#'
#' @return
#' Error if exact number is met; otherwise, nothing.
#'
#' @export
#'
#' @examples
#' check_element_num(1:5, 5)
#' \dontrun{
#' check_element_num(1:5, 4)
#' }
check_element_num <- function(x, num) {
  if(length(x) != num) {
    cli::cli_abort(
      "`{substitute(x)}` must have {num} element{?s}, not {length(x)} element{?s}."
    )
  }
}

#' Test if vector is of a specific type
#'
#' @param x Vector
#' @param type Character string of vector type
#'
#' @return
#' Error if type does not match; otherwise, nothing.
#' @export
#'
#' @examples
#' check_type(1, "numeric")
#' \dontrun{
#' check_type("1", "numeric")
#' }
check_type <- function(x, type) {
  if(!inherits(x, type)) {
    cli::cli_abort(paste0(
      "`{substitute(x)}` must be of type {type};  not {typeof(x)}."
    ))
  }
}
