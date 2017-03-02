#' New Haven, CT neighborhoods by tract and block group
#'
#' A named, nested list of lists (of lists, in some cases). Each outer list item represents one neighborhood.
#'
#' @format A list of length 23
#' \describe{
#'   \item{tract}{FIPS code for a Census tract}
#'   \item{block.group}{numeric vector of FIPS codes for block groups}
#' }
"nhv_neighborhoods"
