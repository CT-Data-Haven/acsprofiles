#' Get years available in Census API
#'
#' Convenience function for splitting a span of years based on availability in the Census ACS API
#'
#' @param start_year A four-digit integer for the first year in the desired time period
#' @param end_year A four-digit integer for the last year in the desired time period
#'
#' @return Returns a tbl
#'
#' @details The Census ACS API is currently only available for 2012 onward. This makes it tricky to pull data for a longer period, such as 2010 to 2015. The \code{type} column in the returned dataframe is useful for iterating over years that must be downloaded as csv files, versus those available from the API.
#' @examples
#' years <- make_years_for_trend(2010, 2015)
#' @export
make_years_for_trend <- function(start_year, end_year) {
  csv_years <- if(start_year <= 2011) {
    if(end_year >= 2012) {
      start_year:2011
    } else if(end_year < 2012) {
      start_year:end_year
    } else {
      NULL
    }
  }
  api_years <- if(end_year >= 2012) {
    if(start_year <= 2011) {
      2012:end_year
    } else if(start_year > 2011) {
      start_year:end_year
    } else {
      NULL
    }
  }
  years_df <- dplyr::data_frame(year = c(csv_years, api_years),
                         type = c(purrr::rep_along(csv_years, "csv"), purrr::rep_along(api_years, "api")))
  return(years_df)
}
