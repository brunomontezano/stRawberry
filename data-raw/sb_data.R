# yield
solo_prod <- read.csv("data-raw/solo-prod.csv")
deitado_prod <- read.csv("data-raw/deitado-prod.csv")
pe_prod <- read.csv("data-raw/pe-prod.csv")
dupla_prod <- read.csv("data-raw/dupla-prod.csv")

# number of fruits
solo_frutos <- read.csv("data-raw/solo-frutos.csv")
deitado_frutos <- read.csv("data-raw/deitado-frutos.csv")
pe_frutos <- read.csv("data-raw/pe-frutos.csv")
dupla_frutos <- read.csv("data-raw/dupla-frutos.csv")

# brix degrees
solo_brix <- read.csv("data-raw/solo-brix.csv")
deitado_brix <- read.csv("data-raw/deitado-brix.csv")
pe_brix <- read.csv("data-raw/pe-brix.csv")
dupla_brix <- read.csv("data-raw/dupla-brix.csv")

# create yield dataset
producao <- list(solo_prod, deitado_prod, pe_prod, dupla_prod)

da_prod <- producao %>%
  purrr::map(~ janitor::clean_names(.x)) %>%
  purrr::map(~ .x %>%
      dplyr::mutate(
        alb = (alb_20 + alb_21)/nrow(.x))
  ) %>%
  purrr::map(~ .x %>%
      dplyr::mutate(
        sa = (sa_20 + sa_21)/nrow(.x))
  ) %>%
  purrr::map(~ .x %>%
      dplyr::mutate(
        port = (port_20 + port_21)/nrow(.x))
  ) %>%
  purrr::map(~ .x %>%
      dplyr::select(
        alb, sa, port)
  ) %>%
  purrr::set_names(
    nm = paste0(c("solo", "deitado", "pe", "dupla"))
  ) %>%
  dplyr::bind_rows(.id = "exp") %>%
  tidyr::pivot_longer(
    cols = c(alb, sa, port),
    names_to = "cultivar",
    values_to = "prod"
  )

# create brix degrees numbers of fruits datasets
brix <- list(solo_brix, deitado_brix, pe_brix, dupla_brix)
frutos <- list(solo_frutos, deitado_frutos, pe_frutos, dupla_frutos)

da_brix <- brix %>%
  purrr::set_names("solo", "deitado", "pe", "dupla") %>%
  dplyr::bind_rows(.id = "exp") %>%
  tidyr::pivot_longer(
    cols = c(alb, sa, port),
    names_to = "cultivar",
    values_to = "brix"
  )

da_frutos <- frutos %>%
  purrr::set_names("solo", "deitado", "pe", "dupla") %>%
  dplyr::bind_rows(.id = "exp") %>%
  tidyr::pivot_longer(
    cols = c(alb, sa, port),
    names_to = "cultivar",
    values_to = "frutos"
  )

# combine all datasets together
da_sb <- da_prod %>%
  dplyr::bind_cols(
    da_brix %>% dplyr::select(brix)
    ) %>%
  dplyr::bind_cols(
    da_frutos %>% dplyr::select(frutos)
  ) %>%
  dplyr::filter(prod < 2000) %>% # outlier (happened because of ants)
  dplyr::mutate(
    exp = dplyr::case_when(
    exp == "deitado" ~ "Deitado",
    exp == "dupla" ~ "Dupla",
    exp == "pe" ~ "Em pé",
    exp == "solo" ~ "Solo"
  ),
    cultivar = dplyr::case_when(
      cultivar == "sa" ~ "San Andreas",
      cultivar == "port" ~ "Portola",
      cultivar == "alb" ~ "Albion"
    ))


# export processed data
usethis::use_data(da_sb, overwrite = TRUE)
