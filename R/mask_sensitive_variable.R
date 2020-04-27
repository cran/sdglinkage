#' Replace the firstnames with values from another database.
#'
#' \code{replace_firstname} replaces the firstname in \code{dataset} with firstname from
#'     another database (see \code{\link{firstname_uk}} and \code{\link{firstname_us}})
#'     in case they are too sensitive.
#'
#' @param dataset A data frame of the dataset.
#' @param country A string variable with a default of 'uk'. It is either
#'     'uk' or 'us'.
#' @param age_dependency A logical variable with a default of TRUE.
#' @param gender_dependency A logical variable with a default of TRUE.
#' @param race_dependency A logical variable with a default of FALSE.
#' @return A data frame of the \code{dataset} with the firstname column being replaced
#'     by another firstname database.
#' @examples
#' df <- data.frame(sex=sample(c('male', 'female'), 30, replace = TRUE))
#' df <- add_variable(df, "nhsid")
#' df <- add_variable(df, "firstname", country = "uk", gender_dependency= TRUE, age_dependency = FALSE)
#' replace_firstname(df, country = 'us', age_dependency = FALSE)
#'
#' @export
replace_firstname <- function(dataset, country = 'uk',
                              age_dependency = TRUE,
                              gender_dependency = TRUE,
                              race_dependency = FALSE) {
  if (country == 'uk'){
    if (age_dependency){
      if (gender_dependency){

        if (any(tolower(colnames(dataset)) == "sex"))
        {
          sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "sex"]
        } else if (any(tolower(colnames(dataset)) == "gender"))
        {
          sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "gender"]
        } else
        {
          stop("either sex or gender will be accepted")
        }

        if (any(tolower(colnames(dataset)) == "dob"))
        {
          age_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "dob"]
          dataset$birthyear = '1996'
          for (i in 1:nrow(dataset))
          {
            dataset$birthyear[i] <- as.numeric(substr(dataset[i, age_var_name], 1, 4))
            if (dataset$birthyear[i] < 1996)
            {
              dataset$birthyear[i] <- 1996
            } else if (dataset$birthyear[i] > 2018)
            {
              dataset$birthyear[i] <- 2018
            }
          }
        } else
        {
          stop("please ensure there is a variable called dob")
        }

        birthyear_cat = unique(dataset['birthyear'])
        birthyear_cat$birthyear = as.numeric(birthyear_cat$birthyear)
        sex_cat = unique(dataset[sex_var_name])
        sex_cat$sex = as.character(sex_cat$sex)

        for(i in 1: nrow(birthyear_cat)){
          for(j in 1: nrow(sex_cat)){

            name_rank = data.frame(count=sort(table(dataset[dataset$sex == sex_cat[j,1] & dataset$birthyear == birthyear_cat[i, 1],]$firstname), decreasing=TRUE))
            if (nrow(name_rank)!= 0){
              if (nrow(name_rank) == 1){
                name_rank[1,1] = row.names(name_rank)
              }
              else{
                name_rank$count.Var1 = as.character(name_rank$count.Var1)
              }

              for (k in 1: nrow(name_rank)){

                dataset[dataset$firstname == name_rank[k,1] &
                          dataset$sex ==sex_cat[j,1]&
                          dataset$birthyear == birthyear_cat[i,1],]$firstname =
                  sdglinkage::firstname_uk[sdglinkage::firstname_uk$birthyear == birthyear_cat[i, 1] &
                                             sdglinkage::firstname_uk$sex == sex_cat[j, 1],]$firstname[k]
              }
            }
          }
        }

        dataset$birthyear <- NULL
      }
      else {
        if (any(tolower(colnames(dataset)) == "dob"))
        {
          age_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "dob"]
          dataset$birthyear = '1996'
          for (i in 1:nrow(dataset))
          {
            dataset$birthyear[i] <- as.numeric(substr(dataset[i, age_var_name], 1, 4))
            if (dataset$birthyear[i] < 1996)
            {
              dataset$birthyear[i] <- 1996
            } else if (dataset$birthyear[i] > 2018)
            {
              dataset$birthyear[i] <- 2018
            }
          }
        } else
        {
          stop("please ensure there is a variable called dob")
        }

        birthyear_cat = unique(dataset['birthyear'])
        birthyear_cat$birthyear = as.numeric(birthyear_cat$birthyear)

        for(i in 1: nrow(birthyear_cat)){
          name_rank = data.frame(count=sort(table(dataset[dataset['birthyear'] == birthyear_cat[i,1],]$firstname), decreasing=TRUE))
          if (nrow(name_rank)!= 0){
            if (nrow(name_rank) == 1){
              name_rank[1,1] = row.names(name_rank)
            }
            else{
              name_rank$count.Var1 = as.character(name_rank$count.Var1)
            }

            for (k in 1: nrow(name_rank)){
              dataset[dataset$firstname == name_rank[k,1] & dataset['birthyear'] ==birthyear_cat[i,1],]$firstname =sdglinkage::firstname_uk[sdglinkage::firstname_uk['birthyear'] == birthyear_cat[i,1],]$firstname[k]
            }
          }
        }
        dataset$birthyear <- NULL
      }
    } else if (gender_dependency){
      if (any(tolower(colnames(dataset)) == "sex"))
      {
        sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "sex"]
      } else if (any(tolower(colnames(dataset)) == "gender"))
      {
        sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "gender"]
      } else
      {
        stop("either sex or gender will be accepted")
      }

      sex_cat = unique(dataset[sex_var_name])
      sex_cat$sex = as.character(sex_cat$sex)
      for(j in 1: nrow(sex_cat)){
        name_rank = data.frame(count=sort(table(dataset[dataset$sex == sex_cat[j,1],]$firstname), decreasing=TRUE))
        if (nrow(name_rank)!= 0){
          if (nrow(name_rank) == 1){
            name_rank[1,1] = row.names(name_rank)
          } else{
            name_rank$count.Var1 = as.character(name_rank$count.Var1)
          }
          for (k in 1: nrow(name_rank)){
            dataset[dataset$firstname == name_rank[k,1] & dataset$sex ==sex_cat[j,1],]$firstname =
              sdglinkage::firstname_uk[sdglinkage::firstname_uk['sex'] ==sex_cat[j, 1],]$firstname[k]
          }
        }
      }
    }
    else {
      name_rank = data.frame(count=sort(table(dataset$firstname), decreasing=TRUE))
      if (nrow(name_rank)!= 0){
        if (nrow(name_rank) == 1){
          name_rank[1,1] = row.names(name_rank)
        }
        else{
          name_rank$count.Var1 = as.character(name_rank$count.Var1)
        }
        for (k in 1: nrow(name_rank)){
          dataset[dataset$firstname == name_rank[k,1],]$firstname =
            sdglinkage::firstname_uk$firstname[k]
        }
      }
    }
  }
  else if (country == 'us'){
    if (race_dependency){
      if (gender_dependency){
        if (any(tolower(colnames(dataset)) == "sex"))
        {
          sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "sex"]
        } else if (any(tolower(colnames(dataset)) == "gender"))
        {
          sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "gender"]
        } else
        {
          stop("either sex or gender will be accepted")
        }

        if (any(tolower(colnames(dataset)) == "race"))
        {
          race_var_name <- colnames(dataset)[tolower(colnames(dataset)) ==
                                          "race"]
        } else if (any(tolower(colnames(dataset)) == "ethnicty"))
        {
          race_var_name <- colnames(dataset)[tolower(colnames(dataset)) ==
                                          "ethnicty"]
        } else
        {
          stop("either race or ethnicty will be accepted")
        }

        dataset$race_value = 'White (not Hispanic)'

        for (i in 1:nrow(dataset))
        {
          if (grepl(substr(dataset[i, race_var_name], 1, 3), "White (not Hispanic)") ||
              dataset[i, race_var_name] == 5)
          {
            dataset$race_value[i] <- "White (not Hispanic)"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "American Indian or Native Alaskan")
                     || dataset[i, race_var_name] == 1)
          {
            dataset$race_value[i] <- "American Indian or Native Alaskan"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Black (not Hispanic)")
                     || dataset[i, race_var_name] == 3)
          {
            dataset$race_value[i] <- "Black (not Hispanic)"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Asian or Pacific Islander")
                     || dataset[i, race_var_name] == 2)
          {
            dataset$race_value[i] <- "Asian or Pacific Islander"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Middle-Eastern, Arabic")
                 || dataset[i, race_var_name] == 6)
          {
            dataset$race_value[i] <- "Middle-Eastern, Arabic"
          } else{
            dataset$race_value[i] <- "Hispanic"
          }
        }
        race_cat = unique(dataset['race_value'])
        sex_cat = unique(dataset[sex_var_name])
        sex_cat$sex = as.character(sex_cat$sex)

        for(i in 1: nrow(race_cat)){
          for(j in 1: nrow(sex_cat)){

            name_rank = data.frame(count=sort(table(dataset[((dataset$sex == sex_cat[j,1]) & (dataset$race_value == race_cat[i, 1])),]$firstname), decreasing=TRUE))
            if (nrow(name_rank)!= 0){
              if (nrow(name_rank) == 1){
                name_rank[1,1] = row.names(name_rank)
              }
              else{
                name_rank$count.Var1 = as.character(name_rank$count.Var1)
              }
              for (k in 1: nrow(name_rank)){

                dataset[dataset$firstname == name_rank[k,1] & dataset$sex == sex_cat[j,1] & dataset$race_value ==
                          race_cat[i, 1],]$firstname =
                  sdglinkage::firstname_us[sdglinkage::firstname_us$race == race_cat[i, 1]
                               & sdglinkage::firstname_us$sex == sex_cat[j,1], ]$firstname[k]
              }
            }
          }
        }
        dataset$race_value <- NULL
      }
      else {
        if (any(tolower(colnames(dataset)) == "race"))
        {
          race_var_name <- colnames(dataset)[tolower(colnames(dataset)) ==
                                          "race"]
        } else if (any(tolower(colnames(dataset)) == "ethnicty"))
        {
          race_var_name <- colnames(dataset)[tolower(colnames(dataset)) ==
                                          "ethnicty"]
        } else
        {
          stop("either race or ethnicty will be accepted")
        }
        dataset$race_value = 'White (not Hispanic)'

        for (i in 1:nrow(dataset))
        {
          if (grepl(substr(dataset[i, race_var_name], 1, 3), "White (not Hispanic)") ||
              dataset[i, race_var_name] == 5)
          {
            dataset$race_value[i] <- "White (not Hispanic)"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "American Indian or Native Alaskan")
                     || dataset[i, race_var_name] == 1)
          {
            dataset$race_value[i] <- "American Indian or Native Alaskan"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Black (not Hispanic)")
                     || dataset[i, race_var_name] == 3)
          {
            dataset$race_value[i] <- "Black (not Hispanic)"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Asian or Pacific Islander")
                     || dataset[i, race_var_name] == 2)
          {
            dataset$race_value[i] <- "Asian or Pacific Islander"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Middle-Eastern, Arabic")
                     || dataset[i, race_var_name] == 6)
          {
            dataset$race_value[i] <- "Middle-Eastern, Arabic"
          } else{
            dataset$race_value[i] <- "Hispanic"
          }
        }
        race_cat = unique(dataset['race_value'])
        for(i in 1: nrow(race_cat)){
          name_rank = data.frame(count=sort(table(dataset[dataset$race_value == race_cat[i, 1],]$firstname), decreasing=TRUE))
          if (nrow(name_rank)!= 0){
            if (nrow(name_rank) == 1){
              name_rank[1,1] = row.names(name_rank)
            }
            else{
              name_rank$count.Var1 = as.character(name_rank$count.Var1)
            }
            for (k in 1: nrow(name_rank)){
              dataset[dataset$firstname == name_rank[k,1] & dataset$race_value ==race_cat[i, 1],]$firstname =sdglinkage::firstname_us[sdglinkage::firstname_us$race == race_cat[i, 1],]$firstname[k]
            }
          }
        }
        dataset$race_value <- NULL
      }
    } else if (gender_dependency){
      if (any(tolower(colnames(dataset)) == "sex"))
      {
        sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "sex"]
      } else if (any(tolower(colnames(dataset)) == "gender"))
      {
        sex_var_name <- colnames(dataset)[tolower(colnames(dataset)) == "gender"]
      } else
      {
        stop("either sex or gender will be accepted")
      }

      sex_cat = unique(dataset[sex_var_name])
      sex_cat$sex = as.character(sex_cat$sex)

      for(j in 1: nrow(sex_cat)){
        name_rank = data.frame(count=sort(table(dataset[dataset$sex == sex_cat[j,1],]$firstname), decreasing=TRUE))
        if (nrow(name_rank)!= 0){
          if (nrow(name_rank) == 1){
            name_rank[1,1] = row.names(name_rank)
          }
          else{
            name_rank$count.Var1 = as.character(name_rank$count.Var1)
          }
          for (k in 1: nrow(name_rank)){
            dataset[dataset$firstname == name_rank[k,1] & dataset$sex ==sex_cat[j,1],]$firstname =
              sdglinkage::firstname_us[sdglinkage::firstname_us['sex'] ==sex_cat[j, 1],]$firstname[k]
          }
        }
      }
    }
    else {
      name_rank = data.frame(count=sort(table(dataset$firstname), decreasing=TRUE))
      if (nrow(name_rank)!= 0){
        if (nrow(name_rank) == 1){
          name_rank[1,1] = row.names(name_rank)
        }
        else{
          name_rank$count.Var1 = as.character(name_rank$count.Var1)
        }
        for (k in 1: nrow(name_rank)){
          dataset[dataset$firstname == name_rank[k,1],]$firstname =
            sdglinkage::firstname_us$firstname[k]
        }
      }
    }
  }
  return(dataset)
}





#' Replace the lastnames with values from another database.
#'
#' \code{replace_lastname} replaces the lastname in \code{dataset} with lastname from
#'     another database (see \code{\link{lastname_uk}} and \code{\link{lastname_us}})
#'     in case they are too sensitive.
#'
#' @param dataset A data frame of the dataset.
#' @param country A string variable with a default of 'uk'. It is either
#'     'uk' or 'us'.
#' @param race_dependency A logical variable with a default of FALSE.
#' @return A data frame of the \code{dataset} with the lastname column being replaced
#'     by another lastname database.
#' @examples
#' df <- data.frame(sex=sample(c('male', 'female'), 100, replace = TRUE))
#' df$race <- sample(1:6, 100, replace = TRUE)
#' df <- add_variable(df, "nhsid")
#' df <- add_variable(df, "dob", age_dependency = FALSE)
#' df <- add_variable(df, "firstname", country = "uk", gender_dependency= TRUE, age_dependency = TRUE)
#' df <- add_variable(df, "lastname", country = "uk", gender_dependency= TRUE, age_dependency = TRUE)
#' df$firstname <-as.character(df$firstname)
#' df$lastname <-as.character(df$lastname)
#' replace_lastname(df, country = 'uk')
#' replace_lastname(df, country = 'us', race_dependency =TRUE)
#'
#' @export
replace_lastname <- function(dataset, country = 'uk',
                              race_dependency = FALSE) {
  if (country == 'uk'){
      name_rank = data.frame(count=sort(table(dataset$lastname), decreasing=TRUE))
      if (nrow(name_rank)!= 0){
        if (nrow(name_rank) == 1){
          name_rank[1,1] = row.names(name_rank)
        }
        else{
          name_rank$count.Var1 = as.character(name_rank$count.Var1)
        }
        for (k in 1: nrow(name_rank)){
          dataset[dataset$lastname == name_rank[k,1],]$lastname =
            sdglinkage::lastname_uk$surname[k]
        }
      }
  }
  else if (country == 'us'){
    if (race_dependency){
        if (any(tolower(colnames(dataset)) == "race"))
        {
          race_var_name <- colnames(dataset)[tolower(colnames(dataset)) ==
                                          "race"]
        } else if (any(tolower(colnames(dataset)) == "ethnicty"))
        {
          race_var_name <- colnames(dataset)[tolower(colnames(dataset)) ==
                                          "ethnicty"]
        } else
        {
          stop("either race or ethnicty will be accepted")
        }

        dataset$race_value = 'White (not Hispanic)'

        for (i in 1:nrow(dataset))
        {
          if (grepl(substr(dataset[i, race_var_name], 1, 3), "White (not Hispanic)") ||
              dataset[i, race_var_name] == 5)
          {
            dataset$race_value[i] <- "White (not Hispanic)"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "American Indian or Native Alaskan")
                     || dataset[i, race_var_name] == 1)
          {
            dataset$race_value[i] <- "American Indian or Native Alaskan"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Black (not Hispanic)")
                     || dataset[i, race_var_name] == 3)
          {
            dataset$race_value[i] <- "Black (not Hispanic)"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Asian or Pacific Islander")
                     || dataset[i, race_var_name] == 2)
          {
            dataset$race_value[i] <- "Asian or Pacific Islander"
          } else if (grepl(substr(dataset[i, race_var_name], 1, 3), "Middle-Eastern, Arabic")
                     || dataset[i, race_var_name] == 6)
          {
            dataset$race_value[i] <- "Middle-Eastern, Arabic"
          } else{
            dataset$race_value[i] <- "Hispanic"
          }
        }
        race_cat = unique(dataset['race_value'])



        for(i in 1: nrow(race_cat)){

          name_rank = data.frame(count=sort(table(dataset[dataset$race_value == race_cat[i, 1],]$lastname), decreasing=TRUE))
          if (nrow(name_rank)!= 0){
            if (nrow(name_rank) == 1){
              name_rank[1,1] = row.names(name_rank)
            }
            else{
              name_rank$count.Var1 = as.character(name_rank$count.Var1)
            }

            for (k in 1: nrow(name_rank)){

              # if(any(is.na(dataset$lastname))){
              #   dataset[is.na(dataset$lastname),]$lastname = sample(sdglinkage::lastname_us[sdglinkage::lastname_us$race == race_cat[i, 1],]$lastname,1)
              # }
              dataset[dataset['race_value'] == race_cat[i,1] & dataset$lastname == name_rank[k,1], 'lastname'] <-
                sdglinkage::lastname_us[sdglinkage::lastname_us$race == race_cat[i, 1],]$lastname[k]
            }
          }
        }
        dataset$race_value <- NULL
    } else {
      name_rank = data.frame(count=sort(table(dataset$lastname), decreasing=TRUE))
      if (nrow(name_rank)!= 0){
        if (nrow(name_rank) == 1){
          name_rank[1,1] = row.names(name_rank)
        } else{
          name_rank$count.Var1 = as.character(name_rank$count.Var1)
        }
        for (k in 1: nrow(name_rank)){
          dataset[dataset$lastname == name_rank[k,1],]$lastname =
            sdglinkage::lastname_us$lastname[k]
        }
      }
    }
  }
  return(dataset)
}


#' Replace nhsid with another random nhsid.
#'
#' \code{replace_nhsid} replaces the nhsid in \code{dataset} with another random nhsid
#'     in case they are too sensitive.
#'
#' @param dataset A data frame of the dataset.
#' @return A data frame of the \code{dataset} with the nhsid column being replaced
#'     by random nhsid.
#' @examples
#' df <- data.frame(sex=sample(c('male', 'female'), 100, replace = TRUE))
#' df$race <- sample(1:6, 100, replace = TRUE)
#' df <- add_variable(df, "nhsid")
#' replace_nhsid(df)
#'
#' @export
replace_nhsid <- function(dataset) {
  dataset = add_variable(dataset, 'nhsid')
  return(dataset)
}
