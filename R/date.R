#' Generate a record of date of birth.
#'
#' \code{gen_dob} randomly return a record of date of birth.
#'
#' @param start A Date variable with a default of '1900-01-01'.
#' @param end A Date variable with a default of '2020-01-01'.
#' @return The output is a record of date of birth in Date format between
#'   1900-01-01 and 2020-01-01. If \code{start} is given, the return date
#'   will be between the updated start date and 2020-01-01. If \code{end}
#'   is also given, the return date will be between the updated start date
#'   and updated end date.
#' @examples
#' gen_dob()
#' gen_dob(start = "1995-01-01")
#' gen_dob(end = "2000-01-01")
#' gen_dob(start = "1909-01-01", end = "2000-01-01")
#'
#' @export
gen_dob <- function(start = "1900-01-01", end = "2020-01-01")
{
  start <- as.Date(start)
  end <- as.Date(end)
  return(as.character(as.Date(sample.int(end - start, 1), origin = start)))
}


