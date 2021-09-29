#' Compare experiments as interaction term in two-way ANOVA with cultivars
#'
#' @param var Variable to be compared.
#' @param experiments Experiments to be compared as vector of length 2.
#'
#' @return A data frame containing metrics about the ANOVA model.
#' @export
#'
compareExperiments <- function(var, experiments) {

  subset <- da_sb %>%
    dplyr::filter(exp %in% experiments)

mod <- stats::aov(
  stats::as.formula(paste0(var, "~ exp * cultivar")),
  data = subset
)

output <- summary(mod) %>%
  unclass() %>%
  as.data.frame() %>%
  dplyr::rename("Gl" = "Df",
    "Soma dos quadrados" = "Sum.Sq",
    "Quadrados m\u00e9dios" = "Mean.Sq",
    "F" = "F.value",
    "p" = "Pr..F.")

return(output)

}
