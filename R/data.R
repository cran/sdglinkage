#' UK addresses.
#'
#' A dataset with 10,000 UK addresses extracted from code{\link{extract_address}}, which
#' uses an API \code{\link[PostcodesioR:random_postcode]{random_postcode}} to sample a
#' real UK address from \url{https://api.postcodes.io/random/postcodes}.
#'
#' @format A data frame with 5 variables: \code{postcode}, \code{country},
#'   \code{primary_care_trust}, \code{longitude} and \code{latitude}.
"address_uk"


#' Adult dataset.
#'
#' The Adult dataset was extracted from the US Census database in 1994; it contains
#' 48,842 individual records with 13 personal variables. It is often used as a
#' prediction task to determine whether a person makes over $50,000 a year given
#' personal information.
#'
#' @format A data frame with 13 variables: \code{age}, \code{workclass},
#'   \code{marital_status}, \code{occupation}, \code{relationship},
#'   \code{race}, \code{sex}, \code{capital_gain}, \code{capital_loss},
#'   \code{hours_per_week}, \code{native_country} and \code{income}.
"adult"


#' Baby birth first names in England and Wales.
#'
#' Full baby birth name data provided by the ONS. This includes all names with at
#' least 5 uses in England and Wales from 1996 to 2018. The frenquency was calculated
#' by the number of uses in each name divided by the number of birth population
#' within each birth year. Details can be found in
#' \url{https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/bulletins/babynamesenglandandwales/2018/relateddata}.
#'
#' @format A data frame with 4 variables:
#' \describe{
#' \item{\code{firstname}}{first name}
#' \item{\code{freq}}{probability of being named as \code{firstname} as a \code{sex} and born at \code{birthyear}}
#' \item{\code{sex}}{gender}
#' \item{\code{birthyear}}{the year was born}.
#' }
"firstname_uk"


#' Last names in UK,
#'
#' UK last name dataset was provided by the ONS. The frenquency was calculated
#' by the number of uses in each name divided by the number of the population
#' within the dataset.
#'
#' @format A data frame with 2 variables: \code{surname} and \code{freq}.
"lastname_uk"



#' First names in the US census.
#'
#' The US firstname database was extracted from \code{\link[randomNames:randomNamesData]{randomNamesData}}.
#' Its origin is the US census.
#'
#' @format A data frame with 4 variables:
#' \describe{
#' \item{\code{firstname}}{first name}
#' \item{\code{freq}}{probability of being named as \code{firstname} as a \code{sex} and is \code{race}}
#' \item{\code{sex}}{gender}
#' \item{\code{race}}{1) American Indian or Native Alaskan, 2) Asian or Pacific Islander,
#'     3) Black (not Hispanic), 4) Hispanic, 5) White (not Hispanic) and
#'     6) Middle-Eastern, Arabic.}.
#' }
"firstname_us"


#' Last names in the US census.
#'
#' The US lastname database was extracted from \code{\link[randomNames:randomNamesData]{randomNamesData}}.
#' Its origin is the US census.
#'
#' @format A data frame with 3 variables:
#' \describe{
#' \item{\code{lastname}}{last name}
#' \item{\code{freq}}{probability of being named as \code{lastname} as a \code{sex} and is \code{race}}
#' \item{\code{race}}{1) American Indian or Native Alaskan, 2) Asian or Pacific Islander,
#'     3) Black (not Hispanic), 4) Hispanic, 5) White (not Hispanic) and
#'     6) Middle-Eastern, Arabic.}.
#' }
"lastname_us"



#' First name variants in the UK.
#'
#' A record of first name variants in the UK, provided by ONS.
#'
#' @format A data frame with 3 variables:
#' \describe{
#' \item{\code{forename}}{the reference name}
#' \item{\code{forename2}}{the variant of the \code{forename}}
#' \item{\code{freq}}{probability of entering \code{forename2} as a variant of \code{forename}}.
#' }
"firstname_uk_variant"


#' Last name variants in the UK.
#'
#' A record of last name variants in the UK, provided by ONS.
#'
#' @format A data frame with 3 variables:
#' \describe{
#' \item{\code{lastname1}}{the reference name}
#' \item{\code{lastname2}}{the variant of the \code{lastname1}}
#' \item{\code{freq}}{probability of entering \code{lastname2} as a variant of \code{lastname1}}.
#' }
"lastname_uk_variant"


#' Look up table of Optical Character Recognition (OCR) errors.
#'
#' A list of OCR errors that may happen, provided by Febrl.
#'
#' @format A data frame with 3 variables:
#' \describe{
#' \item{\code{postion}}{the position of the error within a string}
#' \item{\code{orgpat}}{the original pat}
#' \item{\code{newpat}}{the error pat of \code{orgpat} that is
#'     misrecogonised by the OCR system}.
#' }
"ocr_rules"



#' Look up table of phonetic errors.
#'
#' A list of phonetic errors that may happen, provided by Febrl.
#'
#' @format A data frame with 7 variables:
#' \describe{
#' \item{\code{where}}{the position of the error within a string, can be one of: 'ALL','START','END','MIDDLE'}
#' \item{\code{orgpat}}{the original pat}
#' \item{\code{newpat}}{the error pat of \code{orgpat} that is misheard}
#' \item{\code{precond}}{pre-condition (default 'None') can be 'V' for vowel or 'C' for consonant}
#' \item{\code{postcond}}{post-condition (default 'None') can be 'V' for vowel or 'C' for consonant}
#' \item{\code{existcond}}{exist-condition (default 'None')}
#' \item{\code{startcond}}{start-condition (default 'None')}.
#' }
"pho_rules"
