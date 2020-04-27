#' Encode phonetic error to a string.
#'
#' \code{get_transformation_pho} randomly assign a Phonetic error to a string.
#'     This function was converted from the Python code in Febrl (developed by
#'     Agus Pudjijono in 2008, refers to reference \url{https://link.springer.com/chapter/10.1007/978-3-642-01307-2_47}.
#'
#' @param string A string.
#' @return It returns the \code{string} with a randomly assgined phonetic error following
#'    rules extracted in the pho_rules dataset. It also comes with the change log of the
#'    transformation.
#' @examples
#' get_transformation_pho('how are you?')
#'
#' @export
get_transformation_pho <- function(string)
{
  if (string == "")
  {
    return(paste0(string, "empyt string"))
  }

  changesstr2 <- ""
  workstr <- string
  pho_rules <- sdglinkage::pho_rules
  for (i in 1:nrow(pho_rules))
  {
    tmp <- do_pho_replacement(string, pho_rules[i, 1], pho_rules[i,
                                                                 2], pho_rules[i, 3], pho_rules[i, 4], pho_rules[i, 5], pho_rules[i,
                                                                                                                                  6], pho_rules[i, 7])
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
    workstr <- paste0(workstr, "_lack_of_record, no suitable pho transformation")
  }
  return(workstr)
}




#' Replace a string with its phonetic error.
#'
#' \code{do_pho_replacement} replace a string with its phonetic error.
#'
#' @param s A string.
#' @param where A string. The location of the pat, it can be one of: 'ALL','START','END','MIDDLE'.
#' @param orgpat A string. The original pat.
#' @param newpat A string. The new pat.
#' @param precond A string. Pre-condition (default 'None') can be 'V' for vowel or 'C' for consonant.
#' @param postcond A string. Post-condition (default 'None') can be 'V' for vowel or 'C' for consonant.
#' @param existcond A string. Exist-condition (default 'None').
#' @param startcond A string. Start-condition (default 'ALL').
#' @return It returns a new pat.
#'
#' @export
do_pho_replacement <- function(s, where, orgpat, newpat, precond, postcond,
                               existcond, startcond)
{
  vowels <- "aeiouy"
  tmpstr <- s
  changesstr <- ""

  start_search <- 0  # Position from where to start the search
  pat_len <- nchar(orgpat)
  stop <- FALSE

  # As long as pattern is in string
  z <- 0
  while ((grepl(orgpat, substr(tmpstr, start_search, nchar(tmpstr)))) &
         (stop == FALSE) & (z < 1000))
  {
    z <- z + 1
    pat_start <- gregexpr(pattern = orgpat, substr(tmpstr, start_search +
                                                     1, nchar(tmpstr)))[[1]][1] + start_search
    str_len <- nchar(tmpstr)

    # Check conditions of previous and following character
    OKpre <- FALSE  # Previous character condition
    OKpre1 <- FALSE  # Previous character1 condition
    OKpre2 <- FALSE  # Previous character2 condition

    OKpost <- FALSE  # Following character condition
    OKpost1 <- FALSE  # Following character1 condition
    OKpost2 <- FALSE  # Following character2 condition

    OKexist <- FALSE  # Existing pattern condition
    OKstart <- FALSE  # Existing start pattern condition

    index <- 0

    if (precond == "None")
    {
      OKpre <- TRUE
    } else if (pat_start > 0)
    {
      if (((precond == "V") & (grepl(substr(tmpstr, pat_start -
                                            1, pat_start - 1), vowels))) | ((precond == "C") & (!grepl(substr(tmpstr,
                                                                                                              pat_start - 1, pat_start - 1), vowels))))
      {
        OKpre <- TRUE
      } else if (grepl(";", precond))
      {
        if (grepl("/", precond))
        {
          rls <- as.list(strsplit(precond, "/")[[1]])
          rl1 <- as.list(strsplit(rls[[1]], ";")[[1]])

          if (as.numeric(rl1[[2]]) < 0)
          {
            index <- pat_start + as.numeric(rl1[[2]])
          } else
          {
            index <- pat_start + (nchar(orgpat) - 1) + as.numeric(rl1[[2]])
          }

          i <- 3

          if (rl1[[1]] == "n")
          {

            while (i < (length(rl1) + 1))
            {
              if (substr(tmpstr, index, (index + nchar(rl1[[i]])) -
                         1) == rl1[[i]])
              {
                OKpre1 <- FALSE
                break
              } else
              {
                OKpre1 <- TRUE
              }
              i <- i + 1
            }
          } else
          {


            while (i < (length(rl1) + 1))
            {

              if (substr(tmpstr, index, (index + nchar(rl1[[i]])) -
                         1) == rl1[[i]])
              {
                OKpre1 <- TRUE
                break
              }
              i <- i + 1

            }
          }
          rl2 <- as.list(strsplit(rls[[2]], ";")[[1]])

          if (as.numeric(rl2[[2]]) < 0)
          {
            index <- pat_start + as.numeric(rl2[[2]])
          } else
          {
            index <- pat_start + (nchar(orgpat) - 1) + as.numeric(rl2[[2]])
          }

          i <- 3
          if (rl2[[1]] == "n")
          {
            while (i < (length(rl2) + 1))
            {
              if (substr(tmpstr, index, (index + nchar(rl2[[i]])) -
                         1) == rl2[[i]])
              {
                OKpre2 <- FALSE
                break
              } else
              {
                OKpre2 <- TRUE
              }
              i <- i + 1
            }
          } else
          {
            while (i < (length(rl2) + 1))
            {
              if (substr(tmpstr, index, (index + nchar(rl2[[i]])) -
                         1) == rl2[[i]])
              {
                OKpre2 <- TRUE
                break
              }
              i <- i + 1
            }
          }


          OKpre <- OKpre1 & OKpre2

        } else
        {
          rl <- as.list(strsplit(precond, ";")[[1]])
          #-
          if (as.numeric(rl[[2]]) < 0)
          {
            index <- pat_start + as.numeric(rl[[2]])
          } else
          {
            index <- pat_start + (nchar(orgpat) - 1) + as.numeric(rl[[2]])
          }

          i <- 3
          if (rl[[1]] == "n")
          {
            while (i < (length(rl) + 1))
            {
              if (substr(tmpstr, index, (index + nchar(rl[[i]])) -
                         1) == rl[[i]])
              {
                OKpre <- FALSE
                break
              } else
              {
                OKpre <- TRUE
              }
              i <- i + 1
            }
          } else
          {
            while (i < (length(rl) + 1))
            {

              if (substr(tmpstr, index, (index + nchar(rl[[i]])) -
                         1) == rl[[i]])
              {
                OKpre <- TRUE
                break
              }
              i <- i + 1
            }
          }
        }
      }
    }

    if (postcond == "None")
    {
      OKpost <- TRUE
    } else
    {
      pat_end <- pat_start + pat_len

      if (pat_end < str_len)
      {
        if (((postcond == "V") & (grepl(substr(tmpstr, pat_end,
                                               pat_end), vowels))) | ((postcond == "C") & (!grepl(substr(tmpstr,
                                                                                                         pat_end, pat_end), vowels))))
        {
          OKpost <- TRUE
        } else if (grepl(";", postcond))
        {
          if (grepl("/", postcond))
          {
            rls <- as.list(strsplit(postcond, "/")[[1]])
            rl1 <- as.list(strsplit(rls[[1]], ";")[[1]])

            if (as.numeric(rl1[[2]]) < 0)
            {
              index <- pat_start + as.numeric(rl1[[2]])
            } else
            {
              index <- pat_start + (nchar(orgpat) - 1) + as.numeric(rl1[[2]])
            }

            i <- 3
            if (rl1[[1]] == "n")
            {
              while (i < (length(rl1) + 1))
              {

                if (substr(tmpstr, index, (index + nchar(rl1[[i]])) -
                           1) == rl1[[i]])
                {
                  OKpost1 <- FALSE
                  break
                } else
                {
                  OKpost1 <- TRUE
                }
                i <- i + 1
              }
            } else
            {
              while (i < (length(rl1) + 1))
              {


                if (substr(tmpstr, index, (index + nchar(rl1[[i]])) -
                           1) == rl1[[i]])
                {
                  OKpost1 <- TRUE
                  break
                }
                i <- i + 1

              }
            }
            rl2 <- as.list(strsplit(rls[[1]], ";")[[2]])
            if (as.numeric(rl1[[2]]) < 0)
            {
              index <- pat_start + as.numeric(rl2[[2]])
            } else
            {
              index <- pat_start + (nchar(orgpat) - 1) + as.numeric(rl2[[2]])
            }

            i <- 3
            if (rl2[[1]] == "n")
            {
              while (i < (length(rl2) + 1))
              {
                if (substr(tmpstr, index, (index + nchar(rl2[[i]])) -
                           1) == rl2[[i]])
                {
                  OKpost2 <- FALSE
                  break
                } else
                {
                  OKpost2 <- TRUE
                }
                i <- i + 1
              }
            } else
            {
              while (i < (length(rl2) + 1))
              {
                if (substr(tmpstr, index, (index + nchar(rl2[[i]])) -
                           1) == rl2[[i]])
                {
                  OKpost2 <- TRUE
                  break
                }
                i <- i + 1
              }
            }

            OKpost <- OKpost1 & OKpost2
          } else
          {
            rl <- as.list(strsplit(postcond, ";")[[1]])
            #-
            if (as.numeric(rl[[2]]) < 0)
            {
              index <- pat_start + as.numeric(rl[[2]])
            } else
            {
              index <- pat_start + (nchar(orgpat) - 1) + as.numeric(rl[[2]])
            }

            i <- 3
            if (rl[[1]] == "n")
            {

              while (i < (length(rl) + 1))
              {
                if (substr(tmpstr, index, (index + nchar(rl[[i]])) -
                           1) == rl[[i]])
                {
                  OKpost <- FALSE
                  break
                } else
                {
                  OKpost <- TRUE
                }
                i <- i + 1
              }
            } else
            {

              while (i < (length(rl) + 1))
              {
                if (substr(tmpstr, index, (index + nchar(rl[[i]])) -
                           1) == rl[[i]])
                {
                  OKpost <- TRUE
                  break
                }
                i <- i + 1
              }
            }
          }
        }
      }
    }


    if (existcond == "None")
    {
      OKexist <- TRUE
    } else
    {
      rl <- as.list(strsplit(existcond, ";")[[1]])
      if (rl[[2]] == "slavo")
      {
        r <- slavo_germanic(s)

        if (rl[[1]] == "n")
        {
          if (r == 0)
          {
            OKexist <- TRUE
          }
        } else
        {
          if (r == 1)
          {
            OKexist <- TRUE
          }
        }
      } else
      {
        i <- 2
        if (rl[[1]] == "n")
        {

          while (i < (length(rl) + 1))
          {
            if (grepl(rl[[i]], s))
            {
              OKexist <- FALSE
              break
            } else
            {
              OKexist <- TRUE
            }
            i <- i + i
          }
        } else
        {

          while (i < (length(rl) + 1))
          {
            if (grepl(rl[[i]], s))
            {
              OKexist <- TRUE
              break
            }
            i <- i + i
          }
        }
      }
    }


    if (startcond == "None")
    {
      OKstart <- TRUE
    } else
    {
      rl <- as.list(strsplit(startcond, ";")[[1]])
      i <- 2
      if (rl[[1]] == "n")
      {

        while (i < (length(rl)))
        {
          if (grepl(rl[[i]], s))
          {
            OKstart <- FALSE
            break
          } else
          {
            OKstart <- TRUE
          }
          i <- i + i
        }
      } else
      {

        while (i < (length(rl)))
        {
          if (substr(s, 1, 1) == rl[[i]])
          {
            OKstart <- TRUE
            break
          }
          i <- i + i
        }
      }
    }



    # Replace pattern if conditions and position OK
    if (((OKpre == TRUE) & (OKpost == TRUE) & (OKexist == TRUE) &
         (OKstart == TRUE)) & (((where == "START") & (pat_start ==
                                                      1)) | ((where == "MIDDLE") & (pat_start > 0) & (pat_start +
                                                                                                      pat_len - 1 < str_len)) | ((where == "END") & (pat_start +
                                                                                                                                                     pat_len - 1 == str_len)) | (where == "ALL")))
    {

      tmpstr <- paste0(substr(tmpstr, 1, pat_start - 1), newpat,
                       substr(tmpstr, pat_start + pat_len, nchar(tmpstr)))
      changesstr <- paste0(",", orgpat, ">", newpat, ">", tolower(where))
      start_search <- pat_start + nchar(newpat)
      if (changesstr != "")
      {
        stop <- TRUE
      }
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




#' Detect if it has slavo transformation.
#'
#' \code{slavo_germanic} detect if a string has slavo transformation.
#'
#' @param str A string.
#' @return It returns 1 or 0.
#'
#' @export
slavo_germanic <- function(str)
{
  if (grepl("w", str) | grepl("k", str) | grepl("cz", str) | grepl("witz",
                                                                   str))
  {
    return(1)
  } else
  {
    return(0)
  }
}
