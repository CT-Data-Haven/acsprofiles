#' Get list of town names
#'
#' Gets a tbl of all towns and counties in a given state
#'
#' @param state Either a number corresponding to the state's FIPS code, or a string with the state's two-letter abbreviation or full name. Defaults to 09, FIPS code for Connecticut
#' @param year Endyear of ACS estimates as a four-digit integer. Defaults to 2015, most recent ACS estimates at time of writing
#'
#' @return Returns a data frame
#' @examples
#' ct_towns <- get_town_names(state = 9)
#' @export
get_town_names <- function(state = 9, year = 2015) {
  towns <- acs::acs.fetch(endyear = year,
                     geography = geo.make(state = state, county = "*", county.subdivision = "*"),
                     table.number = "B01003") %>%
    acs::geography() %>%
    dplyr::filter(is.na(stringr::str_match(NAME, "County subdivisions not defined"))) %>%
    dplyr::mutate(town = stringr::str_extract(NAME, ".+?(?= (town|city))"))
  return(towns)
}
