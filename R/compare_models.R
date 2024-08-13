


#' Build formulas from text
#'
#' @param x Model formula from model output
#' @param all Logical for whether all types of formulas are returned (complete,
#' random, and fixed) or just complete formula is returned
#'
#' @return
#' @export
#'
#' @examples
build_formula <- function(x, all = FALSE) {
  form <- insight::find_formula(x)
  dv_fixed <- form$conditional |>
    deparse()
  fixed <- dv_fixed |>
    gsub(".*~ ","",x = _)
  random <- form$random
  if(!is.null(random)) {
    if(inherits(random, "list")) {
      random <- random |>
        purrr::list_flatten() |>
        as.character()
    } else {
      random <- random |>
        deparse()
    }
    random <- random |>
      sub("~", "", x = _) |>
      paste0("(", x = _, ")", collapse = " + ")
    formula <- paste(dv_fixed, random, sep = " + ")
  } else {
    formula <- dv_fixed
  }
  if(all) {
    output <- list(formula = formula, fixed = fixed, random = random)
  } else {
    output <- formula
  }
  return(output)
}

#' Find the best model from a model comparison
#'
#' @param x Output data frame from `compare_models()`.
#' @param threshold Numeric threshold value for Bayes factors
#'
#' @return
#' @export
#'
#' @examples
find_best_model <- function(x, threshold = 3) {
  first_model <- x$Name[1]
  sorted_models <- x |>
    dplyr::arrange(BIC)
  if(sorted_models$BF[1] >= threshold) {
    best_model_name <- sorted_models$Name[1]
  } else {
    best_model_name <- x$Name[1]
  }
  best_model <- eval(parse(text = best_model_name), envir = parent.frame(3))
  best_form <- build_formula(best_model, all = TRUE)
  formula <- best_form$formula
  fixed <- best_form$fixed
  random <- best_form$random
  output <- list(best_model = best_model, best_model_name = best_model_name, formula = formula, fixed = fixed, random = random)
  return(output)
}


#' Compare a set of models
#'
#' @param ... List of model objects
#' @param reference Numeric value signaling model object number to be used
#' for the reference model (denominator) for Bayes factors.
#'
#' @return
#' @export
#'
#' @examples
compare_models <- function(..., reference = 1) {
  models <- list(...)
  names <- unlist(lapply(substitute(list(...))[-1], deparse))
  names_df <- data.frame(Names = names)
  comp_models <- performance::compare_performance(...)
  test_models <- performance::test_performance(..., reference = reference)
  model_formulas <- models |>
    purrr::map_chr(build_formula) |>
    data.frame(Formula = _)
  model_table <- dplyr::left_join(comp_models, test_models,
                                  by = c("Name", "Model")) |>
    dplyr::bind_cols(names_df) |>
    dplyr::bind_cols(model_formulas) |>
    dplyr::select(Name = Names, Formula, Model, AICc, BIC, BF)
  return(model_table)
}


