#
#' Converts numerals to words
#'
#' Adapted from https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r,
#' which is adapted from John Fox (http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html)
#' In this version, I added an option for capitalization
#'
#' @param x Numeric value
#' @param capital Logical for whether to capitalize first word (defaults to
#' FALSE)
#'
#' @return
#' Returns character string of number converted to words.
#'
#' @export
#'
#' @examples
#' num2words(4)
#' num2words(4, capital = TRUE)
num2words <- function(x, capital = FALSE){
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(sapply(x, helper))
  xx <- helper(x)
  if (capital) {
    stringr::str_to_sentence(xx)
  } else {
    return(xx)
  }
}
