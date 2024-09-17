

#' Extracts model formula and fixed and random effects when present
#'
#' @param x Model object
#' @param all Logical specifying whether to return random and fixed effects
#'
#' @return
#' Returns list of model formula and random and fixed effects
#' @export
#'
#' @examples
#' mod <- lm(mpg ~ disp, data = mtcars)
#' build_formula(mod)
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

#' Generate AICc, BIC, and Bayes factors for model comparison
#'
#' @param ... A list of model objects to compare
#' @param reference Character string for model to use as reference in model
#' comparison
#'
#' @return
#' Returns a table with models, AICc, BIC, and Bayes factors.
#'
#' @export
#'
#' @examples
#' mod1 <- lm(mpg ~ disp, data = mtcars)
#' mod2 <- lm(mpg ~ disp + cyl, data = mtcars)
#' compare_models(mod1, mod2)

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


#' Determines best fitting model
#'
#' @param x Data frame output from `compare_models()`.
#' @param threshold Bayes factor threshold for distinguishing models.
#' @param ... Additional arguments to pass on to internal functions.
#'
#' @return
#' Returns list with best fitting model object, model name, model formula, and fixed and random effects.
#' @export
#'
#' @examples
#' mod1 <- lm(mpg ~ disp, data = mtcars)
#' mod2 <- lm(mpg ~ disp + cyl, data = mtcars)
#' mod_comp <- compare_models(mod1, mod2)
#' find_best_model(mod_comp)
find_best_model <- function(x, threshold = 3, ...) {
  x$BF[is.na(x$BF)] <- 1
  first_model <- x$Name[1]
  sorted_models <- x |>
    dplyr::arrange(desc(BF))
  if(sorted_models$BF[1] >= threshold) {
    best_model_name <- sorted_models$Name[1]
    second_best_model_name <- sorted_models$Name[2]
  } else {
    best_model_name <- x$Name[1]
    second_best_model_name <- x$Name[2]
  }
  best_model <- eval(parse(text = best_model_name), ...)
  best_form <- build_formula(best_model, all = TRUE)
  formula <- best_form$formula
  fixed <- best_form$fixed
  random <- best_form$random
  best_bf <- sorted_models$BF[1]
  second_best_model <- eval(parse(text = second_best_model_name), ...)
  best_2nd_bf <- sorted_models$BF[1] / sorted_models$BF[2]
  output <- list(best_model = best_model, best_model_name = best_model_name, formula = formula, fixed = fixed, random = random, bf = best_bf, bf2nd = best_2nd_bf, second_best_model = second_best_model, second_best_model_name = second_best_model_name)
  return(output)
}

#' Find the best random effects model for intercept
#'
#' @param x Data frame for data
#' @param dv Character string for dependent variable
#' @param rand Character vector of possible random effect variables
#' @param glm Logical indicating whether to use linear models (default = FALSE)
#' or generalized linear models (TRUE).
#'
#' @return
#' Returns a character string for the formula of the best fitting random effects
#' model based on Bayes factors.
#' @export
#'
#' @examples
find_best_random_effect <- function(x, dv, rand, glm = FALSE) {
  null_form <- (paste0(dv, " ~ 1"))
  rands <- paste0("(1 | ", rand , ")")
  singles <- paste(dv, "~", rands)
  combined <- (paste(dv, "~", paste0(rands, collapse = " + ")))
  formulas <- c(singles, combined)
  model_names <- paste0("mod", seq_along(1:length(formulas)))
  if(!glm) {
    mod_null <- lm(as.formula(null_form), data = x)
    mod_other <- map(formulas, \(vec) lmer(as.formula(vec), data = x)) |>
      set_names(nm = model_names)
  } else {
    mod_null <- glm(as.formula(null_form), family = binomial, data = x)
    mod_other <- map(formulas, \(vec) lme4::glmer(as.formula(vec), family = binomial, data = x)) |>
      set_names(nm = model_names)
  }
  models <- c(mod_null = list(mod_null), mod_other)
  # compare_models(map(model_names, \(m) eval(parse(text = m), envir = parent.frame(3))))
  model_comparison <- compare_models(models)
  model_comparison$BF[is.na(model_comparison$BF)] <- 1
  best_model_num <- which(model_comparison$BF == max(model_comparison$BF, na.rm = TRUE))
  best_model <- models[[best_model_num]]
  return(build_formula(best_model))
}
