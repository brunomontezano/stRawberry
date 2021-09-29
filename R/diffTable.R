#' Create table with yield, brix degrees and number of fruits and repetitions differences between cultivars
#'
#' @param experiment Experiment data to be shown in the table. Possible values: "Solo", "Deitado", "Em pé", "Dupla".
#'
#' @return A summary table with yield, brix degrees and number of fruits and repetitions.
#' @export
#'
diffTable <- function(experiment) {

  summary_table <- da_sb %>%
    dplyr::filter(exp == experiment) %>%
    dplyr::group_by(.data[["cultivar"]]) %>%
    dplyr::summarise(media_prod = mean(prod),
      dp_prod = stats::sd(prod),
      brix = mean(brix),
      frutos = mean(frutos),
      repeticoes = dplyr::n())

  return(summary_table)

}
