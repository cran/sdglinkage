#' Generate a linkage file by damaging the gold standard file.
#'
#' \code{damage_gold_standard} damage the \code{gold_standard} file into a linkage files. The
#'     damage actions are instructued by the error flags in \code{syn_error_occurrence}. These
#'     actions are:
#'     \enumerate{
#'     \item {missing}: assign 'NA' to the flagged data point;
#'     \item {del}: randomly delete one charater on the flagged data point;
#'     \item {trans_char}: randomly transpose two neighbouring characters on the flagged data point;
#'     \item {trans_date}: randomly transpose the day and the month of a date on the flagged data point;
#'     \item {insert}: randomly insert one charater to the flagged data point;
#'     \item {typo}: randomly assign a typo error to the flagged data point;
#'     \item {ocr}: randomly assign a ocr error to the flagged data point;
#'     \item {pho}: randomly assign a phonetic error to the flagged data point;
#'     \item {variant}: randomly assign a name variant to the flagged data point.
#'     }
#'
#' @param gold_standard A data frame of the gold standard dataset, see \code{\link{add_variable}}.
#' @param syn_error_occurrence A data frame of one-hot encoded error flags, see \code{\link{bn_flag_inference}}.
#' @return A list of two data frame: i) the linkage_file having the same dimension
#'     as the \code{gold_standard} but some of the variables are damaged; ii) the
#'     error_log records the damages have made on the linkage file.
#' @examples
#' adult_with_flag <- add_random_error(adult[1:50,], prob = c(0.97, 0.03), "age_missing")
#' adult_with_flag <- add_random_error(adult_with_flag, prob = c(0.65, 0.35), "firstname_variant")
#' adult_with_flag <- split_data(adult_with_flag, 70)
#' bn_evidence <- "age >=18 & capital_gain>=0 & capital_loss >=0 &
#'                 hours_per_week>=0 & hours_per_week<=100"
#' bn_learn <- gen_bn_learn(adult_with_flag$training_set, "hc", bn_evidence)
#' dataset_smaller_version <- bn_learn$gen_data
#' syn_dependent <- dataset_smaller_version[, !grepl("flag", colnames(dataset_smaller_version))]
#' gold_standard <- add_variable(syn_dependent, "firstname", country = "uk",
#'                               gender_dependency = TRUE, age_dependency = TRUE)
#' syn_error_occurrence <- bn_flag_inference(dataset_smaller_version, bn_learn$fit_model)
#' linkage_file <- damage_gold_standard(gold_standard, syn_error_occurrence)
#'
#' @export
damage_gold_standard <- function(gold_standard, syn_error_occurrence)
{
  s <- gold_standard
  error_log <- syn_error_occurrence
  for (i in 1:ncol(syn_error_occurrence))
  {
    tmp <- strsplit(colnames(syn_error_occurrence)[i], split = "_")[[1]]
    message(paste("encoding error to: ", colnames(syn_error_occurrence)[i]))
    error_log[, i] <- as.character(error_log[, i])

    if (tmp[2] == "missing")
    {
      s[syn_error_occurrence[, i] == 1, tmp[1]] <- NA
      error_log[syn_error_occurrence[, i] == 1, i] <- NA
    } else if (tmp[2] == "del")
    {
      tmp2 <- as.vector(s[syn_error_occurrence[, i] == 1, tmp[1]])
      s[, tmp[1]] <- as.character(s[, tmp[1]])

      for (j in 1:length(tmp2))
      {
        tmp3 <- strsplit(get_transformation_del(tmp2[j]), split = ",")[[1]]
        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(tmp3[1])
        error_log[syn_error_occurrence[, i] == 1, i][j] <- as.character(tmp3[2])
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])
    } else if (tmp[2] == "trans" && tmp[3]=="char")
    {
      tmp2 <- as.vector(s[syn_error_occurrence[, i] == 1, tmp[1]])
      s[, tmp[1]] <- as.character(s[, tmp[1]])

      for (j in 1:length(tmp2))
      {
        tmp3 <- strsplit(get_transformation_trans_char(tmp2[j]),
                         split = ",")[[1]]
        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(tmp3[1])
        error_log[syn_error_occurrence[, i] == 1, i][j] <- as.character(tmp3[2])
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])
    } else if (tmp[2] == "insert")
    {
      tmp2 <- as.vector(s[syn_error_occurrence[, i] == 1, tmp[1]])
      s[, tmp[1]] <- as.character(s[, tmp[1]])

      for (j in 1:length(tmp2))
      {
        tmp3 <- strsplit(get_transformation_insert(tmp2[j]), split = ",")[[1]]
        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(tmp3[1])
        error_log[syn_error_occurrence[, i] == 1, i][j] <- as.character(tmp3[2])
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])
    } else if (tmp[2] == "trans" && tmp[3] == "date")
    {
      tmp2 <- s[syn_error_occurrence[, i] == 1, tmp[1]]
      s[, tmp[1]] <- as.character(s[, tmp[1]])

      for (j in 1:length(tmp2))
      {
        changeddate <- get_transformation_trans_date(tmp2[j])
        tmp3 <- strsplit(changeddate, split = ",")[[1]]
        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(tmp3[1])
        error_log[syn_error_occurrence[, i] == 1, i][j] <- as.character(tmp3[2])
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])
    } else if (tmp[2] == "typo")
    {
      tmp2 <- as.vector(s[syn_error_occurrence[, i] == 1, tmp[1]])
      s[, tmp[1]] <- as.character(s[, tmp[1]])

      for (j in 1:length(tmp2))
      {
        tmp3 <- strsplit(get_transformation_typo(tmp2[j]), split = ",")[[1]]
        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(tmp3[1])
        error_log[syn_error_occurrence[, i] == 1, i][j] <- as.character(tmp3[2])
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])

    } else if (tmp[2] == "pho")
    {
      tmp2 <- as.vector(s[syn_error_occurrence[, i] == 1, tmp[1]])
      s[, tmp[1]] <- as.character(s[, tmp[1]])

      for (j in 1:length(tmp2))
      {
        tmp3 <- strsplit(get_transformation_pho(tmp2[j]), split = ",")[[1]]
        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(tmp3[1])
        error_log[syn_error_occurrence[, i] == 1, i][j] <- as.character(tmp3[2])
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])
    } else if (tmp[2] == "ocr")
    {
      tmp2 <- as.vector(s[syn_error_occurrence[, i] == 1, tmp[1]])
      s[, tmp[1]] <- as.character(s[, tmp[1]])
      for (j in 1:length(tmp2))
      {
        tmp3 <- strsplit(get_transformation_ocr(tmp2[j]), split = ",")[[1]]
        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(tmp3[1])
        error_log[syn_error_occurrence[, i] == 1, i][j] <- as.character(tmp3[2])
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])
    } else if (tmp[2] == "variant")
    {
      tmp2 <- as.vector(s[syn_error_occurrence[, i] == 1, tmp[1]])
      s[, tmp[1]] <- as.character(s[, tmp[1]])
      tmp_name1 <- sdglinkage::firstname_uk_variant
      tmp_name2 <- sdglinkage::lastname_uk_variant
      colnames(tmp_name2) <- colnames(tmp_name1)
      name_variants <- rbind(tmp_name1, tmp_name2)

      for (j in 1:length(tmp2))
      {
        outputname <- tmp2[j]
        tmp_name <- name_variants[name_variants$forename == outputname,]

        if (nrow(tmp_name) != 0)
        {
          outputname <- tmp_name[sample(nrow(tmp_name), size = 1,
                                        replace = TRUE, prob = tmp_name$freq), 2]
        }

        if (outputname == tmp2[j])
        {
          outputname = paste0(outputname, '_lack_of_record')
          changesstr <- paste0(tmp2[j], ">", outputname)
        } else
        {
          changesstr <- paste0(tmp2[j], ">", outputname)
        }

        s[syn_error_occurrence[, i] == 1, tmp[1]][j] <- as.character(outputname)
        error_log[syn_error_occurrence[, i] == 1, i][j] <- changesstr
      }
      s[, tmp[1]] <- as.character(s[, tmp[1]])
    }
  }
  return(list(linkage_file = s, error_log = error_log))
}
