#' Calculate ACS table
#'
#' Calculates aggregated sums and percentages for groups of ACS variables
#'
#' @param groups A named list. Each item should be a numeric vector of column positions. Names will be used as aggregation terms
#' @param denom A single-column ACS object, or one column of a larger ACS object, to be used as the denominator
#' @param table An ACS object where \code{groups} columns are located
#'
#' @return Returns an \code{\link[acs]{acs-class}} object, as is standard from the \code{acs} package.
#'
#' @examples
#' # Calculate the share of CT population below poverty and below 2x poverty (numerators), out of all residents for whom poverty status is determined (denominator)
#' poverty_table <- acs.fetch(geography = geo.make(state = 9), endyear = 2015, table.number = "C17002", col.names = "pretty")
#' pov_status_determined <- poverty[, 1]
#' pov_groups <- list(
#'   below_poverty = 2:3,
#'   low_income = 2:7
#' )
#' share_in_poverty <- calc_acs_table(pov_groups, pov_status_determined, poverty_table)
#' @export
calc_acs_table <- function(groups, denom, table) {
  if(is.null(names(groups))) stop("groups must be a named list")
  if(!is.acs(denom)) stop("denom must be an ACS object")
  if(!is.acs(table)) stop("table must be an ACS object")
  if(ncol(denom) != 1) stop("denom must be a single column of an ACS object")
  table <- groups %>% purrr::map2(names(groups), ~{
    num <- apply(X = table[, .x], FUN = sum, MARGIN = 2, agg.term = .y)
    per <- acs::divide.acs(num, denom, method = "ratio", F)
    return(acs::cbind.acs(num, per))
  }) %>% purrr::reduce(acs::cbind.acs)
  return(table)
}
