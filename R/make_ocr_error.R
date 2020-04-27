#' Encode OCR error to a string.
#'
#' \code{get_transformation_ocr} randomly assign a Optical Character Recognition (OCR)
#'     error to a string. This function was converted from the Python code in Febrl (
#'     developed by Agus Pudjijono in 2008, refers to reference \url{https://link.springer.com/chapter/10.1007/978-3-642-01307-2_47}.
#'
#' @param string A string.
#' @return It returns the \code{string} with a randomly assgined OCR error following
#'    rules extracted in the ocr_rules dataset. It also comes with the change log of the
#'    transformation.
#' @examples
#' get_transformation_ocr('how are you?')
#'
#' @export
get_transformation_ocr <- function(string)
{
  if (string == "")
  {
    return(paste0(string, "empyt string"))
  }

  workstr <- string
  ocr_rules <- sdglinkage::ocr_rules
  for (i in 1:nrow(ocr_rules))
  {
    if (ocr_rules[i, 2] == "|")
    {
      tmp <- do_ocr_replacement(string, ocr_rules[i, 1], "\\|", ocr_rules[i,
                                                                          3])
    } else
    {
      tmp <- do_ocr_replacement(string, ocr_rules[i, 1], ocr_rules[i,
                                                                   2], ocr_rules[i, 3])
    }
    if (grepl(",", tmp))
    {
      workstr <- paste0(workstr, "//", tmp)
    }
  }

  if (grepl(",", workstr))
  {
    tmp <- as.list(strsplit(workstr, "//"))[[1]]
    workstr <- sample(tmp[2:length(tmp)], 1)
    if (grepl("@", workstr))
    {
      tmp <- as.list(strsplit(workstr, ",")[[1]])
      workstr <- paste0(gsub("@", "", tmp[[1]]), ",", tmp[[2]])
    }
  } else
  {
    workstr <- paste0(workstr, "_lack_of_record, no suitable ocr transformation")
  }
  return(workstr)
}






#' Replace a string with its ocr error.
#'
#' \code{do_ocr_replacement} replace a string with its ocr error.
#'
#' @param s A string.
#' @param where A string. The location of the pat, it can be one of: 'ALL','START','END','MIDDLE'.
#' @param orgpat A string. The original pat.
#' @param newpat A string. The new pat.
#' @return It returns a new pat.
#'
#' @export
do_ocr_replacement <- function(s, where, orgpat, newpat)
{
  tmpstr <- s
  changesstr <- ""
  start_search <- 0  # Position from where to start the search
  if (orgpat == "\\|")
  {
    pat_len <- 1
  } else
  {
    pat_len <- nchar(orgpat)
  }
  stop <- FALSE
  z <- 0
  while ((grepl(orgpat, substr(tmpstr, start_search, nchar(tmpstr)))) &
         (stop == FALSE) & z < 1000)
  {
    z <- z + 1
    pat_start <- gregexpr(pattern = orgpat, substr(tmpstr, start_search +
                                                     1, nchar(tmpstr)))[[1]][1] + start_search
    str_len <- nchar(tmpstr)

    if (((where == "START") & (pat_start == 1)) | ((where == "MIDDLE") &
                                                   (pat_start > 0) & (pat_start + pat_len - 1 < str_len)) |
        ((where == "END") & (pat_start + pat_len - 1 == str_len)) |
        (where == "ALL"))
    {

      tmpstr <- paste0(substr(tmpstr, 1, pat_start - 1), newpat,
                       substr(tmpstr, pat_start + pat_len, nchar(tmpstr)))
      # '\\|' to escape '|' as logical operator
      if (orgpat == "\\|")
      {
        changesstr <- paste0(",", "|", ">", newpat, ">", tolower(where))
      } else
      {
        changesstr <- paste0(",", orgpat, ">", newpat, ">", tolower(where))
      }
      start_search <- pat_start + nchar(newpat)
    } else
    {
      start_search <- pat_start + 1
    }

    if (start_search >= (nchar(tmpstr) - 1))
    {
      stop <- TRUE
    }
  }
  tmpstr <- paste0(tmpstr, changesstr)

  return(tmpstr)
}
