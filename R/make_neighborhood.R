#' Create neighborhood geography
#'
#' Merge Census tracts and block groups into an ACS geo set
#'
#' @param geo_list A nested list of tracts (and, optionally, block groups) for a single neighborhood. Each list item must have a FIPS code for a tract, with the name "tract"; see details for setup
#' @param name Name of the neighborhood. Defaults to "Aggregate"
#' @param state Either a number corresponding to the state's FIPS code, or a string with the state's two-letter abbreviation or full name. Defaults to 09, FIPS code for Connecticut
#' @param county Either a number corresponding to the county's FIPS code (safer), or a string with the county's name. Defaults to 09, FIPS code for New Haven County, Connecticut
#' @param blocks Logical. If \code{TRUE}, expects to find named entries of block groups. Defaults to \code{FALSE}, meaning list items only contain tract codes
#'
#' @return Returns a \code{\link[acs]{geo.set}} object.
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
#' beaver_hills_bg_geo <- make_neighborhood(beaver_hills_bg,
#'   name = "Beaver Hills", state = 9, county = 9, blocks = T)
#'
#' beaver_hills_tracts <- list(
#'   list(tract = 141400)
#' )
#' beaver_hills_tract_geo <- make_neighborhood(beaver_hills_tracts,
#'   name = "Beaver Hills", state = 9, county = 9, blocks = F)
#'
#' @export
make_neighborhood <- function(geo_list, name = "Aggregate", state = 9, county = 9, blocks = F) {
  # throw error if this isn't a list
  if(!is.list(geo_list)) stop("geo_list must be a nested list of tracts (and, optionally, block groups)")

  params <- geo_list %>%
    purrr::map(function(x) {
      # throw error if there isn't a named element tract
      if(!"tract" %in% names(x)) stop("Neighborhoods must have named tract elements")
      p <- list(state = state, county = county, tract = x$tract, combine = T, combine.term = name)
      if(blocks) {
        p$block.group = x$block.group
      }
      return(p)
    })
  geo <- params %>%
    purrr::map(~do.call(acs::geo.make, args = .)) %>%
    purrr::reduce(c, combine = T, combine.term = name)
  return(geo)
}
