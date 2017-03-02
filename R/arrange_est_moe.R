#' Arrange estimate and MOE
#'
#' Neatly arrange estimate and margin of error columns, such that each estimate column is followed by a column with the corresponding margin of error.
#'
#' @param .data A tbl.
#' @param omit An integer or vector of integers corresponding to the position of the column(s) that shouldn't be rearranged. Intended use case is where first 1-2 columns are characters such as names and counties. Defaults to 1.
#'
#' @return Returns a tbl, making it useful in dplyr chaining
#' @export
arrange_est_moe <- function(.data, omit = 1) {
  if(!is.numeric(omit)) stop("omit should be an integer corresponding to a column position")
  if(omit > ncol(.data)) stop("omit should be an integer within the range from 1 to ncol")
  x <- c(1:ncol(.data))[-omit]
  half <- length(x) / 2
  est <- x[1:half]
  moe <- x[(half + 1):length(x)]
  # transpose: works like zip in python, returns vector of positions with est & moe interwoven
  names(.data)[moe] <- names(.data)[est] %>% paste0("_moe")
  arranged <- list(est, moe) %>% purrr::transpose() %>% unlist()
  return(.data %>% dplyr::select(omit, arranged))
}
