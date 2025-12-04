# Super clean and tested functions live here


## My notes ##
# Source the R/functions.R file with < Ctrl-Shift-S >
# Use roxygen documentation  with < Ctrl-Shift-Alt-R >
# To check off all staged documents in the Git then do < Ctrl + A > then press the "space bar" twice
# contral + shift + m (quick command for) |>


#' A function that makes a beautiful with Metabolite - Mean SD
#'
#' @param data
#'
#' @returns A data.frame/tibble.
create_table_descriptive_stats <-
  function(data) {
    data |>
      dplyr::group_by(metabolite) |>
      dplyr::summarise(across(value, list(mean = mean, sd = sd))) |>
      dplyr::mutate(across(tidyselect::where(is.numeric), function(x) formatC(x, digits = 2, format = "f"))) |>
      dplyr::mutate(MeanSD = glue::glue("{value_mean}  ({value_sd})")) |>
      dplyr::select(Metabolite = metabolite, `Mean SD` = MeanSD)
  }



#' A super function that takes a data.frame/tibble and use ggplot2
#' to make histogram for each value, with free sclaes
#'
#' @param data
#'
#' @returns A beautiful histogram object
create_plot_distributions <-
  function(data) {
    data |>
      ggplot2::ggplot(ggplot2::aes(x = value, colour = "pink")) +
      ggplot2::geom_histogram(bins = 40) +
      ggplot2::facet_wrap(dplyr::vars(metabolite), scales = "free")
  }


#' "Askepot Askepot, clean my data!"
#'
#' @param data
#'
#' @returns Very clean data
clean <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>
    dplyr::summarise(value = mean(value), .groups = "keep") |>
    dplyr::ungroup()
}


#' Fun that factorizes the class and center/scale the value column
#'
#' @param data
#'
#' @returns data.frame/tibble

preprocess <- function(data) {
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value)
    )
}


#' Fun that first computes a gl (Generalized Linear Model) for a given metabolite with the formula = class ~ value
#'
#' @param data Datatype should be a tibble
#' @param model Any model formula
#'
#' @returns A tibble
fit_model <- function(data, model) {
  stats::glm(
    formula = model,
    data = data,
    family = binomial
  ) |>
    broom::tidy(exponentiate = TRUE) |>
    dplyr::mutate(
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = everything() # adds the colum  metabolite and model
    )
}


#' Fun that runs the preprocess and the fitmodel, on a predefined metabolite
#'
#' @param data a tibble
#' @param WhatIsYourFavoriteMetabolite  as chr
#'
#' @returns A tibble
create_model_results <- function(data = lipidomics, WhatIsYourFavoriteMetabolite) {
  data |>
    dplyr::filter(metabolite == WhatIsYourFavoriteMetabolite) |>
    preprocess() |>
    fit_model()
}


#' A fun that make to modle formulas
#'
#' @param data
#'
#' @returns A  tibble
fit_all_models <- function(data) {
  list(
    class ~ value, # class as a function of value
    class ~ value + gender + age # class as a function of value, with the covariates; gender and age
  ) |>
    purrr::map(\(model_) fit_model(data, model = model_)) |>
    purrr::list_rbind()
}
