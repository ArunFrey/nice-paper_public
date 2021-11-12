# ---
# Supplementary Analysis: Placebo treatments
# ---

library(tidyverse)
library(estimatr)
library(broom)
library(texreg)

source("./src/vars.R")

theme_set(theme_bw(base_size = 24))

# load data
load("./data/edit/allbus.R")

# specify paths
tables_path <- "output/tables/supp/p_placebo/"
plots_path <- "output/plots/supp/p_placebo/"
models_path <- "output/models/supp/p_placebo/"

# specify models
analysis_a <- allbus


# run models #####
ivs_allbus <- paste0("~ treat + ", paste(vars_allbus, collapse = " + "))

dvs_all <- c(
  "diff_asyl", "neighbor_asyl",
  "angry_asyl_bin", "scared_asyl_bin",
  "pity_asyl_bin", "sympa_asyl_bin",
  "asyl_risk_cohesion_bin", "asyl_risk_safety_bin",
  "asyl_risk_economy_bin", "asyl_risk_state_bin"
)

# GENERATE DATE RANGE FOR PLACEBO MODELS
placebo_min <- min(analysis_a$date) + 29
placebo_max <- event_date - 29
date_range <- seq(from = placebo_min, to = placebo_max, by = "days")

# RUN PLACEBO MODELS: estimate treatment for each date
full_models <- c()

for (i in date_range) {
  placebo_treat <- as.Date(i, origin = "1970-01-01")

  placebo_data <- analysis_a %>%
    mutate(treat = ifelse(date >= placebo_treat, 1, 0)) %>%
    filter(date >= placebo_treat - 29 & date <= placebo_treat + 29) %>%
    filter(date <= event_date)

  fit <- lapply(dvs_all,
    FUN = function(x) {
      estimatr::lm_robust(formula(paste(x, ivs_allbus)),
        data = placebo_data,
        se_type = "HC1"
      )
    }
  )

  names(fit) <- dvs_all

  for (dv in names(fit)) {
    tidy_temp <- tidy(fit[[dv]]) %>%
      mutate(
        placebo = placebo_treat,
        nobs = fit[[dv]]$nobs,
        group = ifelse(
          outcome %in% dvs_emo, "emo",
          ifelse(
            outcome %in% dvs_risk, "risk",
            ifelse(
              outcome %in% dvs_dist, "dist",
              ifelse(
                outcome %in% dvs_host, "host",
                ifelse(
                  outcome %in% dvs_mhealth, "mhealth", NA
                )
              )
            )
          )
        )
      )
    full_models <- rbind(full_models, tidy_temp)
  }
}

full_models <- full_models %>%
  filter(term == "treat") %>%
  rename(estimate_p = estimate)

# save models
saveRDS(full_models, file = paste0(models_path, "models.rds"))
