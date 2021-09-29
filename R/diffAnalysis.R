#' Create a short analysis about the yield in the experiment
#'
#' @param var Experiment to be observed for the analysis.
#' @param experiment Experiment to be observed for the analysis.
#'
#' @return A list with the ANOVA model and the output of Tukey test in brazilian portuguese.
#' @export
#'
diffAnalysis <- function(var, experiment) {

subset <- da_sb %>%
  dplyr::filter(exp == experiment)

mod <- stats::aov(
  stats::as.formula(paste0(var, "~ cultivar")),
  data = subset
  )

aov_output <- summary(mod) %>%
  unclass() %>%
  as.data.frame() %>%
   dplyr::rename("Gl" = "Df",
          "Soma dos quadrados" = "Sum.Sq",
          "Quadrados m\u00e9dios" = "Mean.Sq",
          "F" = "F.value",
          "p" = "Pr..F."
     ) %>%
   tibble::rownames_to_column(var = "VI")

tukey_output <- stats::TukeyHSD(mod, conf.level = .95) %>%
  unclass() %>%
  as.data.frame() %>%
   dplyr::rename("Diferen\u00e7a" = "cultivar.diff",
     "Inferior" = "cultivar.lwr",
     "Superior" = "cultivar.upr",
     "p" = "cultivar.p.adj"
     )

final_output <- list(
  ANOVA = aov_output,
  Tukey = tukey_output
  )

return(final_output)

}
