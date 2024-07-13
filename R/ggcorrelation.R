


#' Plot correlations with statistics
#'
#' @param df Data frame
#' @param x Variable for x-axis (must be in quotes)
#' @param y Variable for y-axis (must be in quotes)
#' @param xlabel Label for x-axis (must be in quotes)
#' @param ylabel Lebel for y-axis (must be in quotes)
#' @param cor_type Correlation type or method (must be "pearson", "spearman", or "kendall")
#' @param side Side of plot to place statistics text
#' @param rank Logical for whether rank should be plotted (TRUE) or not (FALSE)
#'
#' @return
#' Returns a ggplot object, the correlation coefficient, and the Bayes factor
#' @export
#'
#' @examples
#' ggcorrelation(df = mtcars, x = "mpg", y = "disp")
#' ggcorrelation(df = mtcars, x = "mpg", y = "disp", side = "right")
ggcorrelation <- function(df, x, y, xlabel = NULL, ylabel = NULL, cor_type = "pearson", side = "left", rank = FALSE) {
  df2 <- dplyr::select(df, x = dplyr::all_of(x), y = dplyr::all_of(y)) |>  # select only x and y columns and rename
    tidyr::drop_na()
  if(rank) {
    df2$plotx <- rank(df2$x)
    df2$ploty <- rank(df2$y)
  } else {
    df2$plotx <- df2$x
    df2$ploty <- df2$y
  }
  if(is.null(xlabel)) xlabel <- x
  if(is.null(ylabel)) ylabel <- y
  if(cor_type == "pearson") {
    stat_var <- "r"
    if(rank) warning("Are you sure you want to plot the ranks with a Pearson correlation?")
  } else if(cor_type == "spearman") {
    stat_var <- "\u03c1"
  } else if (cor_type == "kendall") {
    stat_var <- "\u03c4"
    if(rank) warning("Are you sure you want to plot the ranks with a Kendall correlation?")
  } else {
    stop("Invalid correlation type")
  }
  xside <- ifelse(side == "right", 0.85, 0.15)
  corr <- correlation::correlation(df2, select = c("x", "y"), method = cor_type, bayesian = TRUE)
  df2 <- dplyr::mutate(df2, y = as.numeric(as.character(y)),
                n = corr$n_Obs,
                r = corr$rho,
                lowci = corr$CI_low,
                highci = corr$CI_high,
                bf = corr$BF)

  # Plot data and linear regression
  corrplot <- df2 |>
    ggplot2::ggplot(ggplot2::aes(x = plotx, y = ploty)) +
    ggplot2::geom_point(shape = 1, size = 3) +  # plot individual data points
    ggplot2::geom_smooth(method = "lm", formula = y ~ x) +  # plot regression line
    ggplot2::labs(x = xlabel, y = ylabel) +  # label x- and y-axis
    ggplot2::geom_text(x = axis_prop(df2$plotx, xside),
              y =  axis_prop(df2$ploty, 0.9),
              label = paste("n = ", df2$n, "\n",
                            stat_var, " = ", cocoon::format_num(df2$r, 2), "\n",
                            # format_p(df2$p, digits = 3, pzero = TRUE, italics = FALSE), "\n",
                            cocoon::format_bf(df2$bf, cutoff = 1000, italics = FALSE, subscript = "")), size = 6) +  # add p-values and Bayes factors
    # {if(study) facet_wrap(vars(study))} +
    ggplot2::theme_bw() +  # change theme
    ggplot2::theme(#text = ggplot2::element_text(family = "Arial"),
          panel.grid = ggplot2::element_blank(),  # remove grid lines
          axis.title = ggplot2::element_text(size = 20),  # set axis label font size
          axis.text = ggplot2::element_text(size = 15),
          aspect.ratio = 1)  # set tick mark label font size
  output <- list(plot = corrplot, coefficient = corr$rho, bf = corr$BF)  # create list of output
  return(output)
}

axis_prop <- function(vec, prop) {
  prop * (max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE)) + min(vec, na.rm = TRUE)
}

