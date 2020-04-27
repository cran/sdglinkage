#' Randomly generate a firstname.
#'
#' \code{gen_firstname} randomly sample a firstname from the selected database:
#'     \enumerate{
#'     \item \code{country} If is 'uk', the function will automatically sample a
#'     firstname that based on the \code{gender} and \code{birthyear}. The uk
#'     firstname database was extracted from
#'     \url{https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/bulletins/babynamesenglandandwales/2018/relateddata}
#'     containing firstnames and their frequencies in England and Wales from 1996 to 2018.
#'     \item If \code{country} is 'us', the function will automatically sample a
#'     firstname that based on the \code{gender} and \code{race}. The us
#'     firstname database was extracted from \code{\link[randomNames:randomNamesData]{randomNamesData}}.
#'     }
#' @param country A string variable with a default of 'uk'. It is either
#'     'uk' or 'us'.
#' @param gender A string variable either 'male' or 'female'.
#' @param birthyear A number from 1996 to 2018. For number smaller than 1996 will assumes
#'     as 1996 and greater than 2018 will assumes aas 2018.
#' @param race A number or a string of the ethnicity code:
#'     1 American Indian or Native Alaskan, 2 Asian or Pacific Islander,
#'     3 Black (not Hispanic), 4 Hispanic, 5 White (not Hispanic) and
#'     6 Middle-Eastern, Arabic.
#' @return A name string.
#' @examples
#' gen_firstname(country = "uk", gender = "male", birthyear = 2013)
#' gen_firstname(country = "us", gender = "male", race = 2)
#' gen_firstname(country = "us", gender = "male", race = 'Hispanic')
#'
#' @export
gen_firstname <- function(country = "uk", gender = NA, birthyear = NA,
                          race = NA)
{

  if (tolower(country) == "uk")
  {
    firstname_uk = sdglinkage::firstname_uk

    if (!is.na(gender))
    {
      if (!is.na(birthyear))
      {
        # since we only have record of 1996 to 2018
        if (birthyear < 1996)
        {
          birthyear <- 1996
        } else if (birthyear > 2018)
        {
          birthyear <- 2018
        }
        outputname <- sample(firstname_uk[firstname_uk$sex == tolower(gender) &
                                            firstname_uk$birthyear == birthyear, 1], size = 1, replace = TRUE,
                             prob = firstname_uk[firstname_uk$sex == tolower(gender) & firstname_uk$birthyear ==
                                                birthyear, 2])
      } else
      {
        outputname <- sample(firstname_uk[firstname_uk$sex == tolower(gender),
                                       1], size = 1, replace = TRUE, prob = firstname_uk[firstname_uk$sex ==
                                                                                        tolower(gender), 2])
      }
    } else
    {
      if (!is.na(birthyear))
      {
        if (birthyear < 1996)
        {
          birthyear <- 1996
        } else if (birthyear > 2018)
        {
          birthyear <- 2018
        }
        outputname <- sample(firstname_uk[firstname_uk$birthyear == birthyear,
                                       1], size = 1, replace = TRUE, prob = firstname_uk[firstname_uk$birthyear ==
                                                                                        birthyear, 2])
      } else
      {
        outputname <- sample(firstname_uk[, 1], size = 1, replace = TRUE,
                             prob = firstname_uk[, 2])
      }
    }
  } else
  {
    firstname_us = sdglinkage::firstname_us

    if (!is.na(race))
    {
      if (race == "1")
      {
        race <- "American Indian or Native Alaskan"
      } else if (race == "2")
      {
        race <- "Asian or Pacific Islander"
      } else if (race == "3")
      {
        race <- "Black (not Hispanic)"
      } else if (race == "4")
      {
        race <- "Hispanic"
      } else if (race == "5")
      {
        race <- "White (not Hispanic)"
      } else if (race == "6")
      {
        race <- "Middle-Eastern, Arabic"
      }

      if (!is.na(gender))
      {
        outputname <- sample(firstname_us[firstname_us$sex == tolower(gender) &
                                            firstname_us$race == race, 1], size = 1, replace = TRUE,
                             prob = firstname_us[firstname_us$sex == tolower(gender) & firstname_us$race ==
                                                race, 2])
      } else
      {
        outputname <- sample(firstname_us[firstname_us$race == race,
                                       1], size = 1, replace = TRUE, prob = firstname_us[firstname_us$race ==
                                                                                        race, 2])
      }
    } else
    {
      if (!is.na(gender))
      {
        outputname <- sample(firstname_us[firstname_us$sex == tolower(gender),
                                       1], size = 1, replace = TRUE, prob = firstname_us[firstname_us$sex ==
                                                                                        tolower(gender), 2])
      } else
      {
        outputname <- sample(firstname_us[, 1], size = 1, replace = TRUE,
                             prob = firstname_us[, 2])
      }
    }
  }

  return(outputname)
}





#' Randomly generate a lastname.
#'
#' \code{gen_lastname} randomly sample a lastname from the selected database:
#'     \enumerate{
#'     \item \code{country} If is 'uk', the function will automatically sample a
#'     lastname. from a extracted lastname database. The lastname database was extracted
#'     from ONS.
#'     \item If \code{country} is 'us', the function will automatically sample a
#'     lastname. that based on the \code{race}. The us lastname database was extracted
#'     from \code{\link[randomNames:randomNamesData]{randomNamesData}}.
#'     }
#' @param country A string variable with a default of 'uk'. It is either
#'     'uk' or 'us'.
#' @param race A number or a string of the ethnicity code:
#'     1 American Indian or Native Alaskan, 2 Asian or Pacific Islander,
#'     3 Black (not Hispanic), 4 Hispanic, 5 White (not Hispanic) and
#'     6 Middle-Eastern, Arabic.
#' @return A name string.
#' @examples
#' gen_lastname(country = "uk")
#' gen_lastname(country = "us", race = 2)
#' gen_lastname(country = "us", race = 'Hispanic')
#'
#' @export
gen_lastname <- function(country = "uk", race = NA)
{
  if (tolower(country) == "uk")
  {
    lastname_uk = sdglinkage::lastname_uk

    outputname <- sample(lastname_uk[, 1], size = 1, replace = TRUE, prob = lastname_uk[,
                                                                                  2])
  } else
  {
    lastname_us = sdglinkage::lastname_us

    if (!is.na(race))
    {
      if (race == "1")
      {
        race <- "American Indian or Native Alaskan"
      } else if (race == "2")
      {
        race <- "Asian or Pacific Islander"
      } else if (race == "3")
      {
        race <- "Black (not Hispanic)"
      } else if (race == "4")
      {
        race <- "Hispanic"
      } else if (race == "5")
      {
        race <- "White (not Hispanic)"
      } else if (race == "6")
      {
        race <- "Middle-Eastern, Arabic"
      }
      outputname <- sample(lastname_us[lastname_us$race == race, 1], size = 1,
                           replace = TRUE, prob = lastname_us[lastname_us$race == race,
                                                           2])
    } else
    {
      outputname <- sample(lastname_us[, 1], size = 1, replace = TRUE,
                           prob = lastname_us[, 2])
    }
  }
  return(outputname)
}


