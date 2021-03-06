#' Make ACS table for list of neighborhoods
#'
#' Makes an ACS table for a list of neighborhoods
#'
#' @param neighborhood_list A nested list of lists of tracts (and, optionally, block groups). Each outer list item should be given the name of its neighborhood, and each inner list item must have a FIPS code for a tract, with the name "tract"; see details for setup
#' @param table.number String corresponding to an ACS table number; this is case-sensitive
#' @param year Endyear of ACS estimates as a four-digit integer
#' @param state Either a number corresponding to the state's FIPS code, or a string with the state's two-letter abbreviation or full name
#' @param county Either a number corresponding to the county's FIPS code (safer), or a string with the county's name. Defaults to 09, FIPS code for New Haven County, Connecticut
#' @param blocks Logical. If \code{TRUE}, expects to find named entries of block groups. Defaults to \code{FALSE}, meaning list items only contain tract codes
#'
#' @return Returns an \code{\link[acs]{acs-class}} object, as is standard from the \code{acs} package.
#'
#' @details
#' \code{make_neighborhood} requires a nested list of lists structure. For neighborhoods that specify block groups, this should take the form:
#' \preformatted{
#' dixwell <- list(
#'   list( tract = 141600, block.group = 1:4 )
#' )
#' }
#' For neighborhoods with only tracts specified: \preformatted{
#' dixwell <- list(
#'   list( tract = 141600 )
#' )
#' }
#' Lists are nested to allow for neighborhoods spanning multiple tracts that may need to be paired with their block groups. Example:
#' \preformatted{
#' downtown <- list(
#'   list(tract = 140100, block.group = 1),
#'   list(tract = 361401, block.group = 1:3),
#'   list(tract = 361402, block.group = 1:2)
#' )
#' }
#'
#' @examples
#' beaver_hills_bg <- list(
#'   list(tract = 141400, block.group = 1:4),
#'   list(tract = 141300, block.group = 2)
#' )
#' beaver_hills_pop <- neighborhood_table(beaver_hills_bg, "B01003", blocks = T)
#' @export
neighborhood_table <- function(neighborhood_list, table.number, year = 2015, state = 9, county = 9, blocks = F) {
  if(!is.list(neighborhood_list)) stop("neighborhood_list must be a named list")
  if(is.null(names(neighborhood_list))) stop("neighborhood_list must be a named list")
  geos <- neighborhood_list %>%
    purrr::map2(names(neighborhood_list),
         ~make_neighborhood(.x, name = .y, state = state, county = county, blocks = blocks)) %>%
    purrr::reduce(c)
  table <- acs::acs.fetch(geography = geos, endyear = year, table.number = table.number, col.names = "pretty")
  acs::geography(table)$NAME <- names(neighborhood_list)
  return(table)
}
