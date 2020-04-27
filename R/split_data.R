#' Split the data into a training_set and a testing_set.
#'
#' \code{split_data} split the data into a training_set
#'     and a testing_set based on the \code{training_percentage}.
#'
#' @param dataset A data frame of the dataset.
#' @param training_percentage A number between 0 and 100 indicating the
#'     percertage of the training dataset.
#'
#' @return A list with two data frame: training_set and testing_set.
#' @examples
#' adult_data <- split_data(adult[1:100,], 70)
#'
#' @export
split_data <- function(dataset, training_percentage)
{
  training_set <- dataset[1:(nrow(dataset) * training_percentage * 0.01), ]
  testing_set <- dataset[-(1:(nrow(dataset) * training_percentage * 0.01)),
                      ]
  return(list(training_set = training_set, testing_set = testing_set))
}
