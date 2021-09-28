#' Plot yield difference
#'
#' @param strata Variable to be used for plot stratification.
#' @param facet Variable to be used for plot faceting.
#'
#' @return Plot containing yield differences.
#' @export
#'
yieldDiffplot <- function(strata, facet) {

  base_plot <- da_sb %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = {{ strata }}, y = prod) +
    ggplot2::geom_bar(stat = "summary", fun = mean, fill = "#6b705c") +
    ggplot2::stat_summary(
      geom = "errorbar",
      fun.data = ggplot2::mean_se,
      width = .5) +
    ggplot2::labs(x = "Cultivar", y = "Yield (grams per plant)")

  if(missing(facet)) {
    final_plot <- base_plot
  } else {
    final_plot <- base_plot + ggplot2::facet_wrap(dplyr::vars({{ facet }}))
  }

  return(final_plot)

}
