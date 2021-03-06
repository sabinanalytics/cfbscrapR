#' Get FPI historical rating data (most recent of each year)
#'
#'
#'
#' @param year Year
#'
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#'
#' cfb_ratings_fpi(year=2018)

cfb_ratings_fpi <- function(year = 2019) {
  current_year <- as.double(substr(Sys.Date(), 1, 4))

  # Small error handling to guide the limits on years
  if (!dplyr::between(as.numeric(year), 2004, current_year)) {
    stop(paste("Please choose year between 2004 and", current_year))
  }

  # Add message according to totals or weeks
  message(
      glue::glue("Scraping FPI totals for {year}!")
  )
# Base URL
  fpi_full_url <- "http://site.web.api.espn.com/apis/fitt/v3/sports/football/college-football/powerindex?region=us&lang=en"

  url <- glue::glue("{fpi_full_url}&season={year}&limit=200")

  raw_json_fpi = fromJSON(url)

  ## get team fpi stats
  get_fpi_data <- function(row_n){
    purrr::pluck(raw_json_fpi, "teams", "categories", row_n, "totals", 1)
  }
  purrr::pluck(raw_json_fpi, "categories", "labels", 1)

  # unnest_wider() name repair is noisy
  # Let's make it quiet with purrr::quietly()
  quiet_unnest_wider <- purrr::quietly(tidyr::unnest_wider)

  purrr::pluck(raw_json_fpi, "teams", "team") %>%
    dplyr::as_tibble() %>%
    dplyr::select(id, nickname, abbreviation, logos, links) %>%
    dplyr::mutate(row_n = dplyr::row_number()) %>%
    dplyr::mutate(data = map(row_n, get_fpi_data)) %>%
    # lots of name_repair here that I am silencing
    quiet_unnest_wider(data) %>%
    purrr::pluck("result") %>%
    purrr::set_names(nm = c(
      "id", "name", "abbr", "logos", "links", "row_n",
      "fpi", "fpi_rk", "trend", "proj_w", "proj_l", "win_out",
      "win_6", "win_div", "playoff", "nc_game", "nc_win",
      "win_conf", "w", "l", "t"
    )) %>%
    dplyr::mutate(year = year) %>%
    dplyr::mutate_at(vars(win_out:win_conf), ~ as.double(str_remove(., "%"))/100 ) %>%
    dplyr::mutate_at(vars(id, fpi:t), as.double) %>%
    dplyr::select(year, dplyr::everything())
}
