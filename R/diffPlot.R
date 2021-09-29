#' Plot yield difference
#'
#' @param var Variable to be plotted.
#' @param strata Variable to be used for plot stratification.
#' @param facet Variable to be used for plot faceting.
#'
#' @return Plot containing yield differences.
#' @export
#'
diffPlot <- function(var, strata, facet) {

  base_plot <- da_sb %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = {{ strata }},
      y = {{ var }}
    ) +
    ggplot2::geom_bar(
      stat = "summary",
      fun = mean,
      fill = "#6b705c"
    ) +
    ggplot2::stat_summary(
      geom = "errorbar",
      fun.data = ggplot2::mean_se,
      width = .5
    ) +
    ggplot2::labs(
      x = "Cultivar"
    )

  if(missing(facet)) {

    final_plot <- base_plot

  } else {

    final_plot <- base_plot +
      ggplot2::facet_wrap(
        dplyr::vars({{ facet }})
      )

  }

  return(final_plot)

}
