#' Encode typographic error to a string.
#'
#' \code{get_transformation_typo} randomly assign a typographic error to a string.
#'     This function was converted from the Python code in Febrl (developed by
#'     Agus Pudjijono in 2008, refers to reference \url{https://link.springer.com/chapter/10.1007/978-3-642-01307-2_47}.
#'
#' @param string A string.
#' @return It returns the \code{string} with a randomly assgined typographic error following
#'    rules extracted in the typo_rules. It also comes with the change log of the
#'    transformation.
#' @examples
#' get_transformation_typo('how are you?')
#'
#' @export
get_transformation_typo <- function(string)
{
  if (string == "")
  {
    return(paste0(string, "empyt string"))
  }
  workstr <- do_typo_replacement(string)
  return(workstr)
}





#' Replace a string with its typo error.
#'
#' \code{do_ocr_replacement} replace a string with its typo error.
#'
#' @param s A string.
#' @return It returns a new pat.
#'
#' @export
do_typo_replacement <- function(s)
{
  tmpstr <- s
  rand_str_index <- sample(1:nchar(s), 1)
  input_char <- substr(s, rand_str_index, rand_str_index)

  typo_error <- function(input_char)
  {
    single_typo_prob <- c(0.4, 0.3)
    names(single_typo_prob) <- c("same_row", "same_col")
    output_char <- ""
    rows <- c("s", "vn", "xv", "sf", "wr", "dg", "fh", "gj", "uo",
              "hk", "jl", "k", "n", "bm", "ip", "o", "w", "et", "ad",
              "ry", "yi", "cb", "qe", "zc", "tu", "x", "2", "13", "24",
              "35", "46", "57", "68", "79", "80", "9")
    names(rows) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i",
                     "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
                     "u", "v", "w", "x", "y", "z", "1", "2", "3", "4", "5",
                     "6", "7", "8", "9", "0")

    cols <- c("qzw", "gh", "df", "erc", "d", "rvc", "tbv", "ybn",
              "k", "umn", "im", "o", "jk", "hj", "l", "p", "a", "f",
              "wxz", "gf", "j", "fg", "s", "sd", "h", "as")

    names(cols) <- c("a", "b", "c", "d", "e", "f", "g", "h", "i",
                     "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
                     "u", "v", "w", "x", "y", "z")

    allkeys <- c("abcdefghijklmnopqrstuvwxyz1234567890")

    rand_num <- stats::runif(1)

    if (rand_num <= single_typo_prob["same_row"])
    {
      if (!is.na(rows[input_char]))
      {
        # random chosen neighbouring key in the same keyboard row
        output_char <- sample(strsplit(rows[input_char], "")[[1]],
                              1)
      } else
      {
        # random chosen key
        output_char <- sample(strsplit(allkeys, "")[[1]], 1)
      }
    } else if (rand_num <= (single_typo_prob["same_row"] + single_typo_prob["same_col"]))
    {
      if (!is.na(cols[input_char]))
      {
        # random chosen neighbouring key in the same keyboard col
        output_char <- sample(strsplit(cols[input_char], "")[[1]],
                              1)
      } else
      {
        # random chosen key
        output_char <- sample(strsplit(allkeys, "")[[1]], 1)
      }
    } else
    {
      # random chosen key
      output_char <- sample(strsplit(allkeys, "")[[1]], 1)
    }

    return(output_char)
  }


  output_char <- typo_error(input_char)
  substr(tmpstr, rand_str_index, rand_str_index) <- output_char
  tmpstr <- paste0(tmpstr, ",", input_char, "<", output_char, "<",
                   rand_str_index)
  return(tmpstr)
}
