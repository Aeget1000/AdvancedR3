# Super clean and tested functions live here


## My notes ##
# Source the R/functions.R file with < Ctrl-Shift-S >
# Use roxygen documentation  with < Ctrl-Shift-Alt-R >


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



#' A super function that takes a data.frame/tibble and use ggplot2 to make histogram for each value, with free sclaes
#'
#' @param data
#'
#' @returns A beautiful histogram object

create_plot_distributions <- function(data) {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = value, colour = "pink")
  ) +
    ggplot2::geom_histogram(bins = 40) +
    ggplot2::facet_wrap(dplyr::vars(metabolite), scales = "free")
}
