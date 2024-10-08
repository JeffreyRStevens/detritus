% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_selection.R
\name{find_best_random_effect}
\alias{find_best_random_effect}
\title{Find the best random effects model for intercept}
\usage{
find_best_random_effect(x, dv, rand, glm = FALSE)
}
\arguments{
\item{x}{Data frame for data}

\item{dv}{Character string for dependent variable}

\item{rand}{Character vector of possible random effect variables}

\item{glm}{Logical indicating whether to use linear models (default = FALSE)
or generalized linear models (TRUE).}
}
\value{
Returns a character string for the formula of the best fitting random effects
model based on Bayes factors.
}
\description{
Find the best random effects model for intercept
}
