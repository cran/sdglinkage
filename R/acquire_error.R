#' Compare two data frames.
#'
#' \code{compare_two_df} compares the \code{vars} of data frames given an \code{uniqueId}.
#'
#' @param df1 Data frame 1.
#' @param df2 Data frame 2.
#' @param vars A list of vector of variables to be compared. In each vector, the first
#'     variable name belongs to \code{df1}, and the second variable name belongs to
#'     \code{df2}.
#' @param uniqueId A string of unique ID that is used to matched \code{df2} with \code{df1}.
#' @return It returns a data frame of 7 variables:
#'     \enumerate{
#'     \item {var.x}: the name of the first variable name in each vector of \code{vars};
#'     \item {var.y}: the name of the second variable name in each vector of \code{vars};
#'     \item {uniqueId}: the unique ID given by \code{uniqueId};
#'     \item {values.x}: the value of the first variable name in each vector of \code{vars};
#'     \item {values.y}: the value of the second variable name in each vector of \code{vars};
#'     \item {row.x}: the row of the \code{values.x} in \code{df1};
#'     \item {row.y}: the row of the \code{values.y} in \code{df2};
#'     }
#' @examples
#' df <- data.frame(firstname_variant=character(100), lastname_variant=character(100))
#' df <- add_variable(df, "nhsid")
#' df <- add_variable(df, "firstname", country = "uk", gender_dependency= FALSE,
#'                    age_dependency = FALSE)
#' df <- add_variable(df, "lastname", country = "uk", gender_dependency= FALSE,
#'                    age_dependency = FALSE)
#' df$firstname_variant <-as.character(df$firstname_variant)
#' df$lastname_variant <-as.character(df$lastname_variant)
#' for (i in 1:nrow(df)){
#'   df$firstname_variant[i] = strsplit(get_transformation_name_variant(df$firstname[i]), ',')[[1]][1]
#'   df$lastname_variant[i] = strsplit(get_transformation_name_variant(df$lastname[i]), ',')[[1]][1]
#' }
#' df1 = df[c('nhsid', 'firstname', 'lastname')]
#' df2 = df[c('nhsid', 'firstname_variant', 'lastname_variant')]
#' df2[1:3, 'firstname_variant'] = NA
#' vars = list(c('firstname', 'firstname_variant'), c('lastname', 'lastname_variant'))
#' diffs.table = compare_two_df(df1, df2, vars, 'nhsid')
#'
#' @export
compare_two_df<- function(df1, df2, vars, uniqueId){
  for (i in 1: length(vars)){
    names(df2)[names(df2) == vars[[i]][2]] <- vars[[i]][1]
  }

  tmp <- summary(arsenal::comparedf(df1, df2, by=uniqueId,
                             tol.factor ='labels',
                             factor.as.char = TRUE,
                             tol.char = 'case'))$diffs.table
  return(tmp)
}


#' Add a column of error flags given  two data frames.
#'
#' \code{compare_two_df} compares the \code{vars} of data frames given an \code{uniqueId}.
#'
#' @param df1 Data frame 1.
#' @param diffs.table A data frame of differnces between two data frames given by \code{\link{compare_two_df}}.
#' @param var_name A string of variable name that we want to check if there is error.
#' @param error_type A string of error type name:
#'     \enumerate{
#'     \item {missing}: if the value of \code{var_name} is NA in \code{df2}, it will be flagged
#'         as 1, otherwise, 0;
#'     \item {del}: if the value of \code{var_name} in \code{df2} equals to \code{var_name} in \code{df1}
#'         with a letter being deleted (see \code{\link{get_transformation_del}}), it will be flagged as 1, otherwise, 0;
#'     \item {trans_char}: if the value of \code{var_name} in \code{df2} equals to \code{var_name} in \code{df1}
#'         with two of its letters' position being transposed (see \code{\link{get_transformation_trans_char}}), it will be flagged as 1, otherwise, 0;
#'     \item {trans_date}: if the value of \code{var_name} in \code{df2} equals to \code{var_name} in \code{df1}
#'         with day and month being transposed (see \code{\link{get_transformation_trans_date}}), it will be flagged as 1, otherwise, 0;
#'     \item {insert}: if the value of \code{var_name} in \code{df2} equals to \code{var_name} in \code{df1}
#'         with an additional letter being inserted (see \code{\link{get_transformation_insert}}), it will be flagged as 1, otherwise, 0;
#'     \item {typo}: if the value of \code{var_name} in \code{df2} equals to \code{var_name} in \code{df1}
#'         with a typo error (see \code{\link{get_transformation_typo}}), it will be flagged as 1, otherwise, 0;
#'     \item {ocr}: if the value of \code{var_name} in \code{df2} equals to \code{var_name} in \code{df1}
#'         with an ocr error (see \code{\link{get_transformation_ocr}}), it will be flagged as 1, otherwise, 0;
#'     \item {pho}: if the value of \code{var_name} in \code{df2} equals to \code{var_name} in \code{df1}
#'         with a phonetic error (see \code{\link{get_transformation_pho}}), it will be flagged as 1, otherwise, 0;
#'     \item {variant}: if the value of \code{var_name} in \code{df2} equals to a variant of \code{var_name} in \code{df1}
#'         (see \code{\link{get_transformation_name_variant}}), it will be flagged as 1, otherwise, 0;
#'     }
#' @return It returns a data frame of \code{df1} with an additional error flag column called \code{var_name}.
#' @examples
#' df <- data.frame(firstname_variant=character(20), lastname_variant=character(20))
#' df <- add_variable(df, "nhsid")
#' df <- add_variable(df, "firstname", country = "uk", gender_dependency= FALSE,
#'                    age_dependency = FALSE)
#' df <- add_variable(df, "lastname", country = "uk", gender_dependency= FALSE,
#'                    age_dependency = FALSE)
#' df$firstname_variant <-as.character(df$firstname_variant)
#' df$lastname_variant <-as.character(df$lastname_variant)
#' for (i in 1:nrow(df)){
#'   df$firstname_variant[i] = strsplit(get_transformation_name_variant(df$firstname[i]), ',')[[1]][1]
#'   df$lastname_variant[i] = strsplit(get_transformation_name_variant(df$lastname[i]), ',')[[1]][1]
#' }
#' df1 = df[c('nhsid', 'firstname', 'lastname')]
#' df2 = df[c('nhsid', 'firstname_variant', 'lastname_variant')]
#' df2[1:3, 'firstname_variant'] = NA
#' vars = list(c('firstname', 'firstname_variant'), c('lastname', 'lastname_variant'))
#' diffs.table = compare_two_df(df1, df2, vars, 'nhsid')
#' df1_with_flags = acquire_error_flag(df1, diffs.table, 'firstname', 'missing')
#' df1_with_flags = acquire_error_flag(df1_with_flags, diffs.table, 'firstname', 'variant')
#' df1_with_flags = acquire_error_flag(df1_with_flags, diffs.table, 'firstname', 'pho')
#' df1_with_flags = acquire_error_flag(df1_with_flags, diffs.table, 'firstname', 'ocr')
#'
#' @export
acquire_error_flag <- function(df1, diffs.table, var_name, error_type){
  new_col_name = paste0(var_name,'_',error_type,'_flag')
  df1$tmp <- 0
  names(df1)[length(df1)] <- new_col_name

  if (error_type=='missing'){
    tmp = diffs.table[diffs.table$var.x==var_name & is.na(diffs.table$values.y),'row.x']
    df1[tmp, new_col_name] = 1
    df1[,new_col_name] = as.factor(df1[,new_col_name])
  }
  else if (error_type == 'variant'){
    # remove those with NA
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    if (tolower(var_name)== 'firstname' || tolower(var_name)== 'forename'){

      tmp_name1 <- sdglinkage::firstname_uk_variant
      tmp_name2 <- sdglinkage::lastname_uk_variant
      colnames(tmp_name2) <- colnames(tmp_name1)
      name_variant <- rbind(tmp_name1, tmp_name2)

      for (i in 1:nrow(tmp)){
        rowid = tmp[i, 'row.x']
        match_names <- name_variant[name_variant$forename == tmp[[i, 'values.x']], ]
        if (tmp[[i, 'values.y']] %in% match_names$forename2 ||
            tmp[[i, 'values.y']] == paste0(tmp[[i, 'values.x']], '_lack_of_record')
            ){
          df1[rowid, new_col_name] = 1
        }
        else{
          df1[rowid, new_col_name] = 0
        }
      }
    }
    else if (tolower(var_name)== 'lastname' || tolower(var_name)== 'surname'){
      tmp_name1 <- sdglinkage::firstname_uk_variant
      tmp_name2 <- sdglinkage::lastname_uk_variant
      colnames(tmp_name2) <- colnames(tmp_name1)
      name_variant <- rbind(tmp_name1, tmp_name2)

      for (i in 1:nrow(tmp)){
        rowid = tmp[i, 'row.x']
        match_names <- name_variant[name_variant$lastname1 == tmp[[i, 'values.x']], ]
        if (tmp[[i, 'values.y']] %in% match_names$lastname2 ||
            tmp[[i, 'values.y']] == paste0(tmp[[i, 'values.x']], '_lack_of_record')
        ){
          df1[rowid, new_col_name] = 1
        }
        else{
          df1[rowid, new_col_name] = 0
        }
      }
    }
    else {
      warning('only firstname/forename or lastname/surname has variant')
    }
    df1[,new_col_name] = as.factor(df1[,new_col_name])

  }
  else if (error_type == 'pho'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    for (i in 1:nrow(tmp)){
      rowid = tmp[i, 'row.x']
      string = tmp[i, 'values.x']

      workstr <- string
      pho_rules <- sdglinkage::pho_rules
      for (j in 1:nrow(pho_rules))
      {
        pho_list <- do_pho_replacement(string, pho_rules[j, 1],
                                       pho_rules[j,2], pho_rules[j, 3],
                                       pho_rules[j, 4], pho_rules[j, 5],
                                       pho_rules[j,6], pho_rules[j, 7])
        if (grepl(",", pho_list))
        {
          workstr <- paste0(workstr, "//", strsplit(pho_list, ',')[[1]][1])
        }
      }

      if (grepl("//", workstr))
      {
        tmplist <- as.list(strsplit(workstr, "//"))[[1]]
        tmplist = tmplist[tmplist != string]

        if (tmp[[i, 'values.y']] %in% tmplist ||
            tmp[[i, 'values.y']] == paste0(tmp[[i, 'values.x']], '_lack_of_record')
        ){
          df1[rowid, new_col_name] = 1
        }
        else{
          df1[rowid, new_col_name] = 0
        }
      }
      else {
        df1[rowid, new_col_name] = 0
      }
    }
    df1[,new_col_name] = as.factor(df1[,new_col_name])

  }
  else if (error_type == 'ocr'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    for (i in 1:nrow(tmp)){
      rowid = tmp[i, 'row.x']
      string = tmp[i, 'values.x']

      workstr <- string
      ocr_rules <- sdglinkage::ocr_rules
      for (j in 1:nrow(ocr_rules))
      {
        if (ocr_rules[j, 2] == "|")
        {
          ocr_list <- do_ocr_replacement(string, ocr_rules[j, 1], "\\|",
                                         ocr_rules[j,3])
        } else
        {
          ocr_list <- do_ocr_replacement(string, ocr_rules[j, 1],
                                    ocr_rules[j,2], ocr_rules[j, 3])
        }

        if (grepl(",", ocr_list))
        {
          workstr <- paste0(workstr, "//", strsplit(ocr_list, ',')[[1]][1])
        }
      }

      if (grepl("//", workstr))
      {
        tmplist <- as.list(strsplit(workstr, "//"))[[1]]
        tmplist = tmplist[tmplist != string]

        if (tmp[[i, 'values.y']] %in% tmplist ||
            tmp[[i, 'values.y']] == paste0(tmp[[i, 'values.x']], '_lack_of_record')
        ){
          df1[rowid, new_col_name] = 1
        } else{
          df1[rowid, new_col_name] = 0
        }
      } else {
        df1[rowid, new_col_name] = 0
      }
    }
    df1[,new_col_name] = as.factor(df1[,new_col_name])

  }
  else if (error_type == 'typo' ||
           error_type == 'insert'  || error_type == 'del'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    for (i in 1:nrow(tmp)){
      rowid = tmp[i, 'row.x']
      string1 = tmp[[i, 'values.x']]
      string2 = tmp[[i, 'values.y']]
      stringlist1 = strsplit(string1, split = "")[[1]]
      stringlist2 = strsplit(string2, split = "")[[1]]

      if (error_type == 'insert' || error_type =='typo'){
        diff = diff_two_strings (stringlist2, stringlist1)
        if (length(diff) == 1 &&
            ((length(stringlist1) + 1) == length(stringlist2)) &&
            error_type == 'insert'
            ){
          df1[rowid, new_col_name] = 1
        }
        else if (length(diff) == 1 &&
            (length(stringlist1) == length(stringlist2)) &&
            error_type == 'typo'
        ){
          df1[rowid, new_col_name] = 1
        }
        else {
          df1[rowid, new_col_name] = 0
        }
      }
      else if (error_type == 'del'){
        diff = diff_two_strings (stringlist1, stringlist2)
        if (length(diff) == 1 &&
            ((length(stringlist1) - 1) == length(stringlist2))){
          df1[rowid, new_col_name] = 1
        }
        else {
          df1[rowid, new_col_name] = 0
        }
      }
    }
    df1[,new_col_name] = as.factor(df1[,new_col_name])

  }
  else if (error_type == 'trans_date'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    for (i in 1:nrow(tmp)){
      rowid = tmp[i, 'row.x']
      date1 = as.character(tmp[[i, 'values.x']])
      date2 = as.character(tmp[[i, 'values.y']])
      month1 <- substr(date1, 6, 7)
      day1 <- substr(date1, 9, 10)
      month2 <- substr(date2, 6, 7)
      day2 <- substr(date2, 9, 10)

      if (month1 == day2 && month2 == day1){
        df1[rowid, new_col_name] = 1
      }
      else {
        df1[rowid, new_col_name] = 0
      }
    }
    df1[,new_col_name] = as.factor(df1[,new_col_name])

  }
  else if (error_type == 'trans_char'){
    tmp = diffs.table[diffs.table$var.x==var_name & !is.na(diffs.table$values.y),]

    for (i in 1:nrow(tmp)){
      rowid = tmp[i, 'row.x']
      string1 = tmp[[i, 'values.x']]
      string2 = tmp[[i, 'values.y']]

      if (check_swap_char(string1, string2)){
        df1[rowid, new_col_name] = 1
      }
      else {
        df1[rowid, new_col_name] = 0
      }
    }
    df1[,new_col_name] = as.factor(df1[,new_col_name])

  }
  return(df1)
}


#' Find all letters in \code{string1} which are not in \code{string2}.
#' \code{diff_two_strings} is adopted from package vecsets function vsetdiff,
#'     it returns all letters in \code{string1} which are not in \code{string2}.
#'
#' @param string1 A string.
#' @param string2 A string.
#' @param multiple A logical variable with a default of TRUE. If multiple is TRUE, it
#'     will non-unique letters, otherwise, only unique letters.
diff_two_strings <-  function (string1, string2, multiple=TRUE)
{
  x <- as.vector(string1)
  y <- as.vector(string2)
  # new code to check for empty sets  here
  if(!length(x)) return(NULL)
  if(!length(y)) return(x)
  # end new code
  xx <- x[!is.na(x)]
  xn <- x[is.na(x)]
  yy <- y[!is.na(y)]
  yn <- y[is.na(y)]
  # removed the if(length) as redundant
  #   if (length(x) || length(y)) {
  if (!multiple) {
    difout <- unique( x[match(x, y, 0L) == 0L])   #original code plus output obj
    #this tapply setup fails on NA, which is why any NA were separated out
  }else {
    # if the output of unlist() is length 0
    # then difout <- xx  (foo[-0]  does naughty things)
    tapout <- unlist(tapply(yy, yy, function(yyy) utils::head(which(xx == yyy[1]), length(yyy) )   )  )
    if(length(tapout)) difout<-xx[-tapout] else difout<- xx
    ndif <- max(0,length(xn)-length(yn) )
    difout<- c(difout, rep(NA,ndif) )
  }
  #        } else  difout <- x
  return(difout)
}

#' Check if two strings are the same after we swaped the position of two letters.
#'
#' \code{check_swap_char} check if two strings are the same after we swaped the
#'      position of two letters.
#'
#' @param string1 A string.
#' @param string2 A string.
#' @return It returns TRUE if two strings are the same after we swaped the
#'     position of two letters, otherwise, it returns FALSE.
check_swap_char <- function(string1, string2){
  stringlist1 = strsplit(string1, split = "")[[1]]
  stringlist2 = strsplit(string2, split = "")[[1]]
  len1 = length(stringlist1)
  len2 = length(stringlist2)

  # Return false if both are not of equal length
  if (len1 != len2){
    return (FALSE)
  }

  # To store indexes of previously mismatched letters
  prev = 0
  curr = 0

  count = 0
  i = 1
  while (i < len1+1){
    # If current character doesn't match
    if (stringlist1[i] != stringlist2[i]){
      # Count number of unmatched character
      count = count + 1
      # If unmatched are greater than 2,
      # then return false
      if (count > 2){
        return (FALSE)
      }

      # Store both unmatched characters of
      # both strings
      prev = curr
      curr = i
    }
    i = i+1
  }

  # Check if previous unmatched of string1
  # is equal to curr unmatched of string2
  # and also check for curr unmatched character,
  # if both are same, then return true
  return (count == 2 && stringlist1[prev] == stringlist2[curr]
          && stringlist1[curr] == stringlist2[prev])
}
