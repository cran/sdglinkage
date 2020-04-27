## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '../') 

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(sdglinkage)
set.seed(1234)

## ------------------------------------------------------------------------
real_gsf = adult[c('age', 'race', 'sex')][1:3000,]
real_gsf_with_flags <- add_random_error(real_gsf, prob = c(0.70, 0.30), "age_missing")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.50, 0.50), "race_missing")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.65, 0.35), "sex_missing")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.90, 0.10), "postcode_trans_char")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.50, 0.50), "firstname_variant")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.50, 0.50), "lastname_variant")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.50, 0.50), "firstname_typo")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.50, 0.50),"firstname_pho")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.50, 0.50), "firstname_ocr")
real_gsf_with_flags <- add_random_error(real_gsf_with_flags, prob = c(0.50, 0.50),"firstname_trans_char")
head(real_gsf_with_flags)

## ------------------------------------------------------------------------
bn_learn <- gen_bn_learn(real_gsf_with_flags, "hc")
syn_dependent <- bn_learn$gen_data[, !grepl("flag", colnames(bn_learn$gen_data))]
head(syn_dependent)

## ------------------------------------------------------------------------
syn_gsf <- add_variable(syn_dependent, "nhsid")
syn_gsf <- add_variable(syn_gsf, "address")
syn_gsf$country <-NULL
syn_gsf$primary_care_trust <-NULL
syn_gsf$longitude <-NULL
syn_gsf$latitude <-NULL
head(syn_gsf)

## ------------------------------------------------------------------------
syn_gsf <- add_variable(syn_gsf, "firstname", country = "uk", gender_dependency = TRUE, age_dependency = TRUE)
syn_gsf <- add_variable(syn_gsf, "lastname", country = "uk")
head(syn_gsf)

## ------------------------------------------------------------------------
syn_error_occurrence1 <- bn_flag_inference(bn_learn$gen_data, bn_learn$fit_model)
head(syn_error_occurrence1)

syn_error_occurrence2 <- bn_flag_inference(bn_learn$gen_data, bn_learn$fit_model)
head(syn_error_occurrence2)

## ------------------------------------------------------------------------
syn_lf1 <- damage_gold_standard(syn_gsf, syn_error_occurrence1)
head(syn_lf1$linkage_file)
head(syn_lf1$error_log)

syn_lf2 <- damage_gold_standard(syn_gsf, syn_error_occurrence2)
head(syn_lf2$linkage_file)

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(reclin)
library(dplyr)
# 'postcode' is used as the blocking variable. 
linked_data_set <- pair_blocking(syn_lf1$linkage_file, syn_lf2$linkage_file, "postcode") %>%
  compare_pairs(by = c("lastname", "firstname", "sex", "race"),
                default_comparator = jaro_winkler(0.8)) %>%
  score_problink(var = "weight") %>%
  select_n_to_m("weight", var = "ntom", threshold = 0) %>%
  link()

## ------------------------------------------------------------------------
# This gives us the statistics of missed match
table(linked_data_set$nhsid.x == linked_data_set$nhsid.y)

# These are records of missed match
head(linked_data_set[linked_data_set$nhsid.x != linked_data_set$nhsid.y,],7)

