#' Checks all files for non-ASCII characters
#'
#' From <https://github.com/DavisVaughan/extrachecks/issues/15>
#'
#' @returns
#' Returns data frame of files and lines with non-ASCII characters.
#'
#' @export
#'
#' @examples
#' nonascii <- check_non_ascii()
check_non_ascii <- function() {
  purrr::map_df(
    .id = "file",
    # list common text files
    .x = fs::dir_ls(
      recurse = TRUE,
      type = "file",
      # ignore images, compressed
      regexp = "\\.(png|ico|rda)$",
      invert = TRUE
    ),
    .f = function(path) {
      x <- readLines(path, warn = FALSE)
      # from tools::showNonASCII()
      asc <- iconv(x, "latin1", "ASCII")
      ind <- is.na(asc) | asc != x
      # make data frame
      if (any(ind)) {
        tibble::tibble(
          row = which(ind),
          line = iconv(x[ind], "latin1", "ASCII", sub = "byte")
        )
      } else {
        tibble::tibble()
      }
    }
  )
}
