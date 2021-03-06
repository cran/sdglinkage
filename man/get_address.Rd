% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/address.R
\name{get_address}
\alias{get_address}
\title{Get an address.}
\usage{
get_address(postcode = NA)
}
\arguments{
\item{postcode}{A string}
}
\value{
The output is a list of 5 variables:
  1) postcode of the UK address, 2) country, 3) primary_care_trust
  4) longitude of the address and 5) latitude of the address.
  #'   If \code{postcode} is given, the return address is an address with
 the defined outward postcode
}
\description{
\code{get_address} get an address using an API from \code{\link[PostcodesioR:random_postcode]{random_postcode}}.
The API sample a real UK address from \url{https://api.postcodes.io/random/postcodes}.
}
\examples{
get_address()
get_address('w3')

}
