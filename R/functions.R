# super clean and tested functions live here
# Source the R/functions.R file with Ctrl-Shift-S

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


