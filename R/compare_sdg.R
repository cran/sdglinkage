#' Compare the performance of generators.
#'
#' \code{compare_sdg} compares the preditive performance of models
#' trained by synthetic data with model trained by real data.
#'
#' This function returns the measured performance of predictive models
#' trained by the synthetic data. We assume good quality synthetic data
#' would allow us to draw the same analytic conclusions as we can draw
#' from real data. Hence, we compare the predictive performance of several
#' machine learning algorithms that are trained with the synthetic data
#' and tested by real data with those trained and tested both by real data.
#'
#' @param learner A learner object from \code{\link[mlr:RLearner]{makeLearners}}.
#' @param measurement A list of performance measurements for \code{\link[mlr:benchmark]{benchmark}}.
#' @param target_var A string of the response variable name.
#' @param real_dataset A list of data frames with a training_set data frame and a
#'     testing_set data frame. You can get this list from \code{\link{split_data}}.
#' @param generated_data1 A data frame of synthetic data 1.
#' @param generated_data2 A data frame of synthetic data 2.
#' @param generated_data3 A data frame of synthetic data 3.
#' @param generated_data4 A data frame of synthetic data 4.
#' @param generated_data5 A data frame of synthetic data 5.
#' @param generated_data6 A data frame of synthetic data 6.
#' @return The output is a \code{\link[mlr:benchmark]{benchmark}} object. It compares the
#'     the preditive performance of selected models trained by the real data and
#'     validated by the testing data with models trained by the generated data and
#'     validated by the testing data.
#' @examples
#' library(mlr)
#' adult_data <- adult[c('age', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week',
#'                       'income')]
#' adult_data <- split_data(adult_data[1:100,], 70)
#' bn_learn <- gen_bn_learn(adult_data$training_set, "hc")
#' lrns <- makeLearners(c("rpart", "logreg"), type = "classif",predict.type = "prob")
#' measurements <- list(acc, ber)
#' bmr <- compare_sdg(lrns,
#'     measurement = measurements,
#'     target_var = "income",
#'     real_dataset = adult_data,
#'     generated_data1 = bn_learn$gen_data)
#' names(bmr$results) <- c("real_dataset","bn_learn")
#' bmr
#'
#' @export
compare_sdg <- function(learner, measurement, target_var, real_dataset,
                           generated_data1, generated_data2 = NA, generated_data3 = NA,
                           generated_data4 = NA, generated_data5 = NA, generated_data6 = NA)
{
  if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
      is.list(generated_data4) && is.list(generated_data5) && is.list(generated_data6))
  {
    new_df0 <- rbind(real_dataset$training_set, real_dataset$testing_set)
    new_df1 <- rbind(generated_data1, real_dataset$testing_set)
    new_df2 <- rbind(generated_data2, real_dataset$testing_set)
    new_df3 <- rbind(generated_data3, real_dataset$testing_set)
    new_df4 <- rbind(generated_data4, real_dataset$testing_set)
    new_df5 <- rbind(generated_data5, real_dataset$testing_set)
    new_df6 <- rbind(generated_data6, real_dataset$testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df0, target = target_var),
                  mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var),
                  mlr::makeClassifTask(data = new_df4, target = target_var),
                  mlr::makeClassifTask(data = new_df5, target = target_var),
                  mlr::makeClassifTask(data = new_df6, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
             is.list(generated_data4) && is.list(generated_data5))
  {
    new_df0 <- rbind(real_dataset$training_set, real_dataset$testing_set)
    new_df1 <- rbind(generated_data1, real_dataset$testing_set)
    new_df2 <- rbind(generated_data2, real_dataset$testing_set)
    new_df3 <- rbind(generated_data3, real_dataset$testing_set)
    new_df4 <- rbind(generated_data4, real_dataset$testing_set)
    new_df5 <- rbind(generated_data5, real_dataset$testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df0, target = target_var),
                  mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var),
                  mlr::makeClassifTask(data = new_df4, target = target_var),
                  mlr::makeClassifTask(data = new_df5, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
             is.list(generated_data4))
  {
    new_df0 <- rbind(real_dataset$training_set, real_dataset$testing_set)
    new_df1 <- rbind(generated_data1, real_dataset$testing_set)
    new_df2 <- rbind(generated_data2, real_dataset$testing_set)
    new_df3 <- rbind(generated_data3, real_dataset$testing_set)
    new_df4 <- rbind(generated_data4, real_dataset$testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df0, target = target_var),
                  mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var),
                  mlr::makeClassifTask(data = new_df4, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3))
  {
    new_df0 <- rbind(real_dataset$training_set, real_dataset$testing_set)
    new_df1 <- rbind(generated_data1, real_dataset$testing_set)
    new_df2 <- rbind(generated_data2, real_dataset$testing_set)
    new_df3 <- rbind(generated_data3, real_dataset$testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df0, target = target_var),
                  mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var),
                  mlr::makeClassifTask(data = new_df3, target = target_var))
  } else if (is.list(generated_data1) && is.list(generated_data2))
  {
    new_df0 <- rbind(real_dataset$training_set, real_dataset$testing_set)
    new_df1 <- rbind(generated_data1, real_dataset$testing_set)
    new_df2 <- rbind(generated_data2, real_dataset$testing_set)

    tasks <- list(mlr::makeClassifTask(data = new_df0, target = target_var),
                  mlr::makeClassifTask(data = new_df1, target = target_var),
                  mlr::makeClassifTask(data = new_df2, target = target_var))
  } else if (is.list(generated_data1))
  {
    new_df0 <- rbind(real_dataset$training_set, real_dataset$testing_set)
    new_df1 <- rbind(generated_data1, real_dataset$testing_set)
    tasks <- list(mlr::makeClassifTask(data = new_df0, target = target_var),
                  mlr::makeClassifTask(data = new_df1, target = target_var))
  }
  rin <- mlr::makeFixedHoldoutInstance(train.inds = 1:nrow(real_dataset$training_set),
                                       test.inds = nrow(real_dataset$training_set):nrow(new_df0),
                                       size = nrow(new_df0))
  # bmr = mlr::benchmark(learner, tasks, rin, measures =
  # parse(text=measurement))
  bmr <- mlr::benchmark(learner, tasks, rin, measures = measurement)
  return(bmr)
}





#' Plot the distribution of a varaible from the synthetic data comparing with the real data.
#'
#' \code{plot_compared_sdg} return a plot of the comparision of
#' the distribution of synthetic data vs real data.
#'
#' @param target_var A string of the comparision variable name.
#' @param training_set A data frame of the training data.
#' @param syn_data_names A string vector of names of the generators.
#' @param generated_data1 A data frame of synthetic data 1.
#' @param generated_data2 A data frame of synthetic data 2.
#' @param generated_data3 A data frame of synthetic data 3.
#' @param generated_data4 A data frame of synthetic data 4.
#' @param generated_data5 A data frame of synthetic data 5.
#' @param generated_data6 A data frame of synthetic data 6.
#' @return The output is a plot of the comparision of the
#'     distribution of synthetic data vs real data. If the
#'     \code{target_var} is discrete, the plot is a bar plot,
#'     If the \code{target_var} is continous, the plot is
#'     a density plot,
#' @examples
#' adult_data <- split_data(adult[1:100,], 70)
#' bn_learn <- gen_bn_learn(adult_data$training_set, "hc")
#' plot_compared_sdg(target_var = "age",
#'     training_set = adult_data$training_set,
#'     syn_data_names = c("bn_learn"),
#'     generated_data1 = bn_learn$gen_data)
#' plot_compared_sdg(target_var = "race",
#'     training_set = adult_data$training_set,
#'     syn_data_names = c("bn_learn"),
#'     generated_data1 = bn_learn$gen_data)
#'
#' @export
plot_compared_sdg <- function(target_var, training_set, syn_data_names,
                               generated_data1, generated_data2 = NA, generated_data3 = NA, generated_data4 = NA,
                               generated_data5 = NA, generated_data6 = NA)
{
  if (is.factor(training_set[, target_var]))
  {
    if (is.list(generated_data1) && is.list(generated_data2) && is.list(generated_data3) &&
        is.list(generated_data4) && is.list(generated_data5) && is.list(generated_data6))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data4[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data5[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data6[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2) &&
               is.list(generated_data3) && is.list(generated_data4) && is.list(generated_data5))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data4[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data5[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2) &&
               is.list(generated_data3) && is.list(generated_data4))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data4[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2) &&
               is.list(generated_data3))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
      df <- merge(df, reshape::melt(table(generated_data3[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1) && is.list(generated_data2))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
      df <- merge(df, reshape::melt(table(generated_data2[target_var])),
                  by = "Var.1", all = T)
    } else if (is.list(generated_data1))
    {
      df <- merge(reshape::melt(table(training_set[target_var])),
                  reshape::melt(table(generated_data1[target_var])), by = "Var.1",
                  all = T)
    }

    colnames(df) <- c(target_var, "Real_dataset", syn_data_names)
    df[is.na(df)] <- 0
    df <- reshape::melt(df, id = c(target_var))
    colnames(df) <- c(target_var, "Data", "Count")
    ggplot2::ggplot(df, ggplot2::aes_string(x = target_var, y = "Count",
                                            fill = "Data")) + ggplot2::geom_bar(position = "dodge", stat = "identity") +
      ggplot2::scale_fill_brewer(palette = "Set3") + ggplot2::theme_minimal()
  } else if (is.numeric(training_set[, target_var]))
  {
    if (is.list(generated_data1))
    {
      df_con <- cbind(training_set[target_var], generated_data1[target_var])
      if (is.list(generated_data2))
      {
        df_con <- cbind(df_con, generated_data2[target_var])
        if (is.list(generated_data3))
        {
          df_con <- cbind(df_con, generated_data3[target_var])
          if (is.list(generated_data4))
          {
            df_con <- cbind(df_con, generated_data4[target_var])
            if (is.list(generated_data5))
            {
              df_con <- cbind(df_con, generated_data5[target_var])
              if (is.list(generated_data6))
              {
                df_con <- cbind(df_con, generated_data6[target_var])
              }
            }
          }
        }
      }
    }

    colnames(df_con) <- c("Real_dataset", syn_data_names)
    df_con <- reshape::melt(df_con)
    colnames(df_con) <- c("Data", "Value")
    df_con <- df_con[!df_con$Value < 0, ]

    df_con$Data <- factor(df_con$Data, levels = c("Real_dataset", syn_data_names))
    ggplot2::ggplot(df_con, ggplot2::aes_string(x = "Value", fill = "Data")) +
      ggplot2::geom_density(alpha = 0.75) + ggplot2::scale_fill_brewer(palette = "Set3") +
      ggplot2::theme_minimal()
    # ggplot2::ggplot(df_con, ggplot2::aes(x = Value, fill = Data)) +
    #   ggplot2::geom_density(alpha = 0.75) + ggplot2::scale_fill_brewer(palette = "Set3") +
    #   ggplot2::theme_minimal()
  }
}
