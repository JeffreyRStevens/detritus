
#' Increment version in DESCRIPTION and CITATION and add new heading to
#' NEWS.md
#'
#' @param level Character string "major", "minor", or "patch"
#' @param addnews Logical indicating whether to add new heading to NEWS.md
#' (default is TRUE).
#'
#' @returns
#' No output returned, but side effect of updating DESCRIPTION,
#' inst/CITATION, and (optionally) NEWS.md files.
#'
#' @export
#'
increment_version <- function(level = NULL,
                              addnews = TRUE) {
  if (is.null(level)) cli::cli_abort("`level` is a required argument.")
  level <- match.arg(level, c("major", "minor", "patch"))

  new_version <- bump_version()[[level]]

  citation_file <- "inst/CITATION"
  if (file.exists(citation_file)) {
    readr::read_file(citation_file) |>
      stringr::str_replace("R package version [0-9]\\.[0-9]\\.[0-9]",
                           paste0("R package version ", new_version)) |>
      readr::write_file(citation_file)
  }

  descrip_file <- "DESCRIPTION"
  if (file.exists(descrip_file)) {
    readr::read_file(descrip_file) |>
      stringr::str_replace("Version: [0-9]\\.[0-9]\\.[0-9]",
                           paste0("Version: ", new_version)) |>
      readr::write_file(descrip_file)
  }

  news_file <- "NEWS.md"
  if (file.exists(news_file) & addnews) {
    pkg_name <- desc::desc(file = new.env(parent = emptyenv())$cur)$get_field("Package")
    news_heading <- paste("#", pkg_name, new_version, "\n\n")
    paste(news_heading, readr::read_file(news_file)) |>
      readr::write_file(news_file)
    cli::cli_alert_success("CITATION, DESCRIPTION, and NEWS updated to version {new_version}")
  } else {
    cli::cli_alert_success("CITATION and DESCRIPTION updated to version {new_version}")
  }
}

#' Update version in CITATION to match DESCRIPTION
#'
#' @param level Character string "major", "minor", or "patch"
#'
#' @returns
#' No output returned, but side effect of updating inst/CITATION file.
#'
#' @export
#'
update_citation_version <- function() {
  new_version <- get_current_version()

  citation_file <- "inst/CITATION"
  if (file.exists(citation_file)) {
    readr::read_file(citation_file) |>
      stringr::str_replace("R package version [0-9]\\.[0-9]\\.[0-9]",
                           paste0("R package version ", new_version)) |>
      readr::write_file(citation_file)
  }
  cli::cli_alert_success("CITATION updated to version {new_version}")
}

#' Get current package version
#'
#' @returns
#' Character string of the current package version
#'
#' @export
#'
get_current_version <- function() {
  proj <- new.env(parent = emptyenv())

  proj_get <- function() proj$cur

  proj_desc <- function(path = proj_get()) {
    desc::desc(file = path)
  }

  proj_desc()$get_field("Version")

}

#' Bump version up by one increment
#'
#' @param ver Character string of current version. Default is to return output
#' of get_current_version().
#'
#' @returns
#' Named vector of incremented major, minor, patch, and developmental levels.
#'
#' @export
#'
#' @examples
#' bump_version("0.1.0")
bump_version <- function(ver = get_current_version()) {
  bump_ <- function(x, ver) {
    d <- desc::desc(text = paste0("Version: ", ver))
    suppressMessages(d$bump_version(x)$get("Version")[[1]])
  }
  bumps <- c("major", "minor", "patch", "dev")
  vapply(bumps, bump_, character(1), ver = ver)
}



