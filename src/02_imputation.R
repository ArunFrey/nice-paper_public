# ---
# Multiple imputation of missing values, using Random Forests
# ---


library(missForest)
library(tidyverse)
library(doParallel)

# register parallel cores
registerDoParallel(cores = 6)

# Allbus #####

# Binary variables and controls
# ID Variable to merge imputed data back with main df
respid <- allbus$respid

# select dvs
vars_select <- c(dvs_allbus[!str_detect(dvs_allbus, "pca")], vars_allbus)

allbus_prep <- allbus %>%
  select(all_of(c(vars_select))) %>%
  mutate_at(vars_select[!vars_select %in% c("diff_asyl", "neighbor_asyl", "age", "intk")], 
            function(x) as.factor(as.character(x))) %>%
  data.frame() 

# impute NAs using random forests
set.seed(42)
allbus_imp <- missForest(allbus_prep, ntree = 100, maxiter = 20, parallelize = "forests")

# obtain error estimate
allbus_imp$OOBerror

# merge with origin
# delete all old variables from original data
allbus_old <- allbus %>%
  select(-all_of(vars_select))

allbus <- cbind(respid, allbus_imp$ximp) %>%
  mutate_if(is.factor, function(x) as.numeric(as.character(x))) %>%
  left_join(allbus_old)


# Factor variable
# ID Variable to merge imputed data back with main df
respid <- allbus$respid

# select vars
vars_select <- dvs_allbus_cont

allbus_prep <- allbus %>%
  select(all_of(c(vars_select))) %>%
  mutate_at(vars_select[!vars_select %in% c("diff_asyl", "neighbor_asyl")], 
            function(x) as.factor(as.character(x))) %>%
  data.frame() 

# impute NAs using random forests
set.seed(42)
allbus_imp <- missForest(allbus_prep, ntree = 100, maxiter = 20, parallelize = "forests")

# obtain error estimate
allbus_imp$OOBerror

# merge with origin
# delete all old variables from original data
allbus_old <- allbus %>%
  select(-all_of(c(vars_select)))

allbus <- cbind(respid, allbus_imp$ximp) %>%
  mutate_if(is.factor, function(x) as.numeric(as.character(x))) %>%
  left_join(allbus_old)

# Refugee #####

# ID Variable to merge imputed data back with main df
pid <- refugee$pid
  
# select variables (add additional auxiliary variables to help the imputation)
vars_select <- c(dvs_refugee, vars_refugee, "mh_nbs", "health_stat")

# Recode cat./bin. variables as factors
refugee_prep <- refugee %>%
  select(all_of(vars_select)) %>%
  mutate_at(vars_select[!vars_select %in% c("mcs", "age", "intk", "mh_nbs")], 
            function(x) as.factor(as.character(x))) %>%
  data.frame() 
  
# IMPUTE MISSINGS USING RANDOM FORESTS
set.seed(42)
refugee_imp <- missForest(refugee_prep, ntree = 100, maxiter = 20, parallelize = "forests")

# OBTAIN ESTIMATE OF ERROR
refugee_imp$OOBerror

# MERGE BACK WITH TREATMENT VARIABLE
# delete all old variables from original refugee data
refugee_old <- refugee %>%
  select(-all_of(vars_select))

refugee <- cbind(pid, refugee_imp$ximp) %>%
  mutate(health_stat = as.character(health_stat)) %>%
  mutate_if(is.factor, function(x) as.numeric(as.character(x))) %>%
  left_join(refugee_old)