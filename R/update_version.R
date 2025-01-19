
#' Update version in CITATION to match DESCRIPTION
#'
#' @param level Character string "major", "minor", or "patch"
#'
#' @returns
#' No output returned, but side effect of updating inst/CITATION file.
#'
#' @export
#'
update_citation_version <- function(level = NULL) {
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
  cli::cli_alert_success("CITATION updated to version {new_version}")
}

proj_version <- function() {
  proj <- new.env(parent = emptyenv())

  proj_get <- function() proj$cur

  proj_desc <- function(path = proj_get()) {
    desc::desc(file = path)
  }

  proj_desc()$get_field("Version")

}

bump_version <- function(ver = proj_version()) {
  bumps <- c("major", "minor", "patch", "dev")
  vapply(bumps, bump_, character(1), ver = ver)
}

bump_ <- function(x, ver) {
  d <- desc::desc(text = paste0("Version: ", ver))
  suppressMessages(d$bump_version(x)$get("Version")[[1]])
}


