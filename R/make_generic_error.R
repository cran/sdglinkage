#' Delete a character randomly.
#'
#' \code{get_transformation_del} randomly delete a character of a string.
#'
#' @param string A string.
#' @return It returns the \code{string} with one of the characters was randomly
#'    deleted. It also comes with the change log of the transformation.
#' @examples
#' get_transformation_del('how are you?')
#'
#' @export
get_transformation_del <- function(string)
{
  # Future improvement: if we have statatiscs that letter 3 has a higher prob of being
  # deleted than letter 2 than letter 5 than....  prob = c(0.1, 0.3, 0.5,
  # 0.1, 0.2) del_position = sample(nchar(string),size = 1, replace =
  # TRUE, prob = prob), otherwise, treat them as equal prob
  #'
  if (string != "" && nchar(string) > 1)
  {
    del_position <- sample(nchar(string), size = 1, replace = TRUE)
    changestr <- paste0(substr(string, 1, del_position - 1),
                        substr(string, del_position + 1, nchar(string)))
    newstr <- paste0(changestr, ",",
                     substr(string, del_position, del_position),
                     ">del>", del_position)
  } else
  {
    newstr <- paste0(string, ",string either empty or shorter than 2 characters")
  }
  return(newstr)
}



#' Randomly transpose two neighbouring characters.
#'
#' \code{get_transformation_trans_char} randomly transpose two neighbouring
#'     characters of a string.
#'
#' @param string A string.
#' @return It returns the \code{string} with two of the neighbouring characters
#'     were randomly transposed. It also comes with the change log of the
#'     transformation.
#' @examples
#' get_transformation_del('how are you?')
#'
#' @export
get_transformation_trans_char <- function(string)
{
  if (string != "" && nchar(string) > 1)
  {
    trans_position <- sample(nchar(string) - 1, size = 1, replace = TRUE)
    changestr <- paste0(substr(string, 1, trans_position - 1),
                        substr(string, trans_position + 1, trans_position + 1),
                        substr(string, trans_position, trans_position),
                        substr(string, trans_position + 2, nchar(string)))
    newstr <- paste0(changestr, ",",
                     substr(string, trans_position, trans_position),
                     substr(string, trans_position + 1, trans_position + 1),
                     ">trans>", trans_position, trans_position + 1)
  } else
  {
    newstr <- paste0(string, ",string either empty or shorter than 2 characters")
  }
  return(newstr)
}





#' Transpose the position of day and month.
#'
#' \code{get_transformation_trans_date} transpose the position of day and month of a
#'     Date format variable.
#'
#' @param date A Date variable.
#' @return The output is the transposition of day and month of \code{date}
#'    and the change log of the transposition. If the day of \code{date}
#'    is greater than 12, the transposition will fail and return the same
#'    \code{date} with a log saying "cannot transposte due to day >12".
#' @examples
#' get_transformation_trans_date("1995-01-11")
#' get_transformation_trans_date("1995-01-13")
#'
#' @export
get_transformation_trans_date <- function(date)
{
  date <- as.character(date)
  year <- substr(date, 1, 4)
  month <- substr(date, 6, 7)
  day <- substr(date, 9, 10)
  newdate <- paste0(year, "-", day, "-", month, ",day>month")

  #
  # if (as.numeric(day) <= 12)
  # {
  #   newdate <- paste0(year, "-", day, "-", month, ",day>month")
  # } else
  # {
  #   newdate <- paste0(date, ",cannot transposte due to day >12")
  # }
  return(newdate)
}



#' Insert a character/digit/space/symbol randomly.
#'
#' \code{get_transformation_del} randomly insert a character/digit/space/symbol
#'     a string.
#'
#' @param string A string.
#' @return It returns the \code{string} with an additional character/digit/space/symbol.
#'     It also comes with the change log of the transformation.
#' @examples
#' get_transformation_insert('how are you?')
#'
#' @export
get_transformation_insert <- function(string)
{
  if (string != "")
  {
    dict <- "abcdefghijklmnopqrstuvwxyz0123456789"
    insertchar <- sample(strsplit(dict, "")[[1]], 1)
    insert_position <- sample(nchar(string), size = 1, replace = TRUE)
    changestr <- paste0(substr(string, 1, insert_position), insertchar,
                        substr(string, insert_position + 1, nchar(string)))
    newstr <- paste0(changestr, ",", insertchar, ">insert>", insert_position)
  } else
  {
    newstr <- paste0(string, ",string is empty")
  }
  return(newstr)
}





#' Randomly assign a name to its variant.
#'
#' \code{get_transformation_name_variant} randomly assign a name to its variant. The
#'     name variant databases are extracted from Febrl.
#'
#' @param string A name string.
#' @return It returns the name variant of \code{string} together with the change log
#'     of the transformation. If no name variant was recorded in the database, it
#'     returns the same name \code{string} with a note of 'no recorded variants'.
#' @examples
#' get_transformation_name_variant("ed")
#' get_transformation_name_variant("shelly")
#' get_transformation_name_variant("MORRIS")
#'
#' @export
get_transformation_name_variant <- function(string)
{
  do_name_replacement <- function(s)
  {
    outputname <- s
    firstname_variant <- sdglinkage::firstname_uk_variant
    lastname_variant <- sdglinkage::lastname_uk_variant
    colnames(lastname_variant) <- colnames(firstname_variant)
    name_variants <- rbind(firstname_variant, lastname_variant)
    tmp <- name_variants[name_variants$forename == s, ]

    if (nrow(tmp) != 0)
    {
      outputname <- tmp[sample(nrow(tmp), size = 1, replace = TRUE,
                               prob = tmp$freq), 2]
    }
    return(as.character(outputname))
  }

  newstr <- do_name_replacement(tolower(string))
  if (newstr == string)
  {
    changesstr <- paste0(newstr, "_lack_of_record,", string, ">", newstr)
  } else
  {
    changesstr <- paste0(newstr, ",", string, ">", newstr)
  }
  return(changesstr)
}

