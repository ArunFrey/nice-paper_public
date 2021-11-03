# ---
# Run all scripts
# ---


# Run analysis ####

source("./vars.R")
source("./01_cleaning_allbus.R")
source("./01_cleaning_soep.R")
source("./02_imputation.R")
source("./03_standardise_vars.R")

save(allbus, file = "./data/edit/allbus.R")
save(allbus_all, file = "./data/edit/allbus_all.R")
save(refugee, file = "./data/edit/refugee.R")
save(refugee_all, file = "./data/edit/refugee_all.R")

# load("./data/edit/allbus.R")
# load("./data/edit/allbus_all.R")
# load("./data/edit/refugee.R")
# load("./data/edit/refugee_all.R")

source("./04_desc.R")
source("./05_main.R")
source("./05_p_placebo.R")
source("./05_r_dvs_other.R")
source("./05_r_east_west.R")
source("./05_r_logit.R")
source("./05_r_matching.R")
source("./05_r_na.R")
source("./05_r_na_group.R")
source("./05_r_no_controls.R")
source("./05_r_reachability.R")
source("./05_r_spec_curve.R")
source("./05_r_time_trend.R")
source("./05_r_treat_alt_periods.R")
source("./05_r_treat_by_week.R")
source("./05_r_treat_no_nice.R")
source("./05_r_treat_ordinal.R")
source("06_plots.R")