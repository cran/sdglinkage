## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '../') 

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(sdglinkage)
set.seed(1234)

## ------------------------------------------------------------------------
adult_data <- split_data(adult, 70)

## ------------------------------------------------------------------------
bn_evidence <- "age >=18 & capital_gain>=0 & capital_loss >=0 & hours_per_week>=0 & hours_per_week<=100"

## ------------------------------------------------------------------------
bn_learn <- gen_bn_learn(adult_data$training_set, "hc", bn_evidence)

## ------------------------------------------------------------------------
plot_bn(bn_learn$structure)

## ------------------------------------------------------------------------
head(bn_learn$gen_data)

## ------------------------------------------------------------------------
bn_structure <- "[native_country][income][age|marital_status:education][sex][race|native_country][marital_status|race:sex][relationship|marital_status][education|sex:race][occupation|education][workclass|occupation][hours_per_week|occupation:workclass][capital_gain|occupation:workclass:income][capital_loss|occupation:workclass:income]"

## ------------------------------------------------------------------------
bn_elicit <- gen_bn_elicit(adult_data$training_set, bn_structure, bn_evidence)

## ------------------------------------------------------------------------
plot_bn(bn_elicit$structure)

## ------------------------------------------------------------------------
head(bn_elicit$gen_data)

## ----results='hide', message=FALSE, warning=FALSE------------------------
cart_elicit <- gen_cart(adult_data$training_set, bn_structure)

## ------------------------------------------------------------------------
head(cart_elicit$gen_data)

## ------------------------------------------------------------------------
compare_cart(adult_data$training_set, cart_elicit$fit_model, c("age", "workclass", "sex"))

## ----results='hide', message=FALSE, warning=FALSE------------------------
plot_compared_sdg(target_var = "race", training_set = adult_data$training_set,
                   syn_data_names = c("CART_elicit", "BN_learn", "BN_elicit"),
                   generated_data1 = cart_elicit$gen_data,
                   generated_data2 = bn_learn$gen_data,
                   generated_data3 = bn_elicit$gen_data)

## ----results='hide', message=FALSE, warning=FALSE------------------------

plot_compared_sdg(target_var = "age", training_set = adult_data$training_set,
                   syn_data_names = c("CART_elicit", "BN_learn", "BN_elicit"),
                   generated_data1 = cart_elicit$gen_data,
                   generated_data2 = bn_learn$gen_data,
                   generated_data3 = bn_elicit$gen_data)

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(mlr)
lrns <- makeLearners(c("rpart", "logreg"), type = "classif",
                     predict.type = "prob")
# lrns <- makeLearners(c("rpart", "logreg", "randomForest"), type = "classif",
#                      predict.type = "prob")
measurements <- list(acc, ber, f1, auc)
bmr <- compare_sdg(lrns, measurement = measurements, target_var = "income",
                      real_dataset = adult_data,
                      generated_data1 = cart_elicit$gen_data,
                      generated_data2 = bn_learn$gen_data,
                      generated_data3 = bn_elicit$gen_data)
names(bmr$results) <- c("Real_dataset", "CART_elicit", "BN_learn", "BN_elicit")

## ------------------------------------------------------------------------
bmr

