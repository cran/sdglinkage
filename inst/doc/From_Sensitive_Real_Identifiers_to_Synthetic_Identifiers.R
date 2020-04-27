## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '../') 

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(sdglinkage)
set.seed(1234)

## ------------------------------------------------------------------------
real_gsf <- data.frame(sex=sample(c('male', 'female'), 100, replace = TRUE))
real_gsf <- add_variable(real_gsf, "nhsid")
real_gsf <- real_gsf[,c(2, 1)]
real_gsf$race <- sample(1:6, 100, replace = TRUE)
real_gsf <- add_variable(real_gsf, "dob", age_dependency = FALSE)
real_gsf <- add_variable(real_gsf, "firstname", country = "uk", gender_dependency= TRUE, age_dependency = TRUE)
real_gsf <- add_variable(real_gsf, "lastname", country = "uk")
head(real_gsf)

## ------------------------------------------------------------------------
error_occurrence_flags <- data.frame(tmp=character(100))
error_occurrence_flags <- add_random_error(error_occurrence_flags, prob = c(0.90, 0.10), "race_missing")
error_occurrence_flags <- add_random_error(error_occurrence_flags, prob = c(0.55, 0.45), "dob_trans_date")
error_occurrence_flags <- add_random_error(error_occurrence_flags, prob = c(0.65, 0.35), "firstname_variant")
error_occurrence_flags <- add_random_error(error_occurrence_flags, prob = c(0.75, 0.25), "lastname_typo")
error_occurrence_flags$tmp <-NULL
real_lf <- damage_gold_standard(real_gsf, error_occurrence_flags)$linkage_file
head(real_lf)

## ------------------------------------------------------------------------
vars = list(c('race', 'race'), c('dob', 'dob'), c('firstname', 'firstname'), c('lastname', 'lastname'))
diffs.table = compare_two_df(real_gsf, real_lf, vars, 'nhsid')
diffs.table

## ------------------------------------------------------------------------
real_gsf_with_flags = acquire_error_flag(real_gsf, diffs.table, 'race', 'missing')
real_gsf_with_flags = acquire_error_flag(real_gsf_with_flags, diffs.table, 'dob', 'trans_date')
real_gsf_with_flags = acquire_error_flag(real_gsf_with_flags, diffs.table, 'firstname', 'variant')
real_gsf_with_flags = acquire_error_flag(real_gsf_with_flags, diffs.table, 'lastname', 'typo')

## ------------------------------------------------------------------------
head(error_occurrence_flags)
acquired_error_flags = real_gsf_with_flags[grep('flag', colnames(real_gsf_with_flags))]
head(acquired_error_flags)

## ------------------------------------------------------------------------
all.equal(acquired_error_flags, error_occurrence_flags)

## ------------------------------------------------------------------------
real_gsf[c(28),]
real_lf[c(28),]

## ------------------------------------------------------------------------
error_occurrence_flags$dob_trans_date_flag[c(28)] = 0
all.equal(acquired_error_flags, error_occurrence_flags)

## ------------------------------------------------------------------------
real_gsf_with_flags_replaced = replace_firstname(real_gsf_with_flags, country = 'us', gender_dependency = TRUE, race_dependency = TRUE)
real_gsf_with_flags_replaced = replace_lastname(real_gsf_with_flags_replaced, country = 'us', race_dependency = TRUE)
real_gsf_with_flags_replaced = replace_nhsid(real_gsf_with_flags_replaced)

## ------------------------------------------------------------------------
head(real_gsf_with_flags[colnames(real_gsf)])

## ------------------------------------------------------------------------
head(real_gsf_with_flags_replaced[colnames(real_gsf)])

## ------------------------------------------------------------------------
# Here we set the variables into the right format for the generator
real_gsf_with_flags_replaced[colnames(real_gsf_with_flags_replaced)] <- lapply(real_gsf_with_flags_replaced[colnames(real_gsf_with_flags_replaced)], factor) 

# We use learned bn to train a generator
bn_learn <- gen_bn_learn(real_gsf_with_flags_replaced, "hc")

# syn_gsf is the generated synthetic gold standard file
syn_gsf = bn_learn$gen_data
head(syn_gsf)


# syn_lf1 and syn_lf2 are the synthetic linkage files that were damaged by the inferred error occurrence in the syn_gsf
syn_error_occurrence_1 <- bn_flag_inference(syn_gsf, bn_learn$fit_model)
syn_lf1 <- damage_gold_standard(syn_gsf, syn_error_occurrence_1)
head(syn_lf1$linkage_file)

syn_error_occurrence_2 <- bn_flag_inference(syn_gsf, bn_learn$fit_model)
syn_lf2 <- damage_gold_standard(syn_gsf, syn_error_occurrence_2)
head(syn_lf2$linkage_file)

## ----message=FALSE, warning=FALSE----------------------------------------
library(reclin)
library(dplyr)

linked_data_set <- pair_blocking(syn_lf1$linkage_file, syn_lf2$linkage_file, "dob") %>%
  compare_pairs(by = c("lastname", "firstname", "sex", "race"),
                default_comparator = jaro_winkler(0.8)) %>%
  score_problink(var = "weight") %>%
  select_n_to_m("weight", var = "ntom", threshold = 0) %>%
  link()

## ------------------------------------------------------------------------
table(linked_data_set$nhsid.x == linked_data_set$nhsid.y)
head(linked_data_set[linked_data_set$nhsid.x != linked_data_set$nhsid.y,],3)

