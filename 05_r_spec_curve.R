# ---
# Supplementary Analysis: Specification curve
# ---

library(tidyverse)
library(estimatr)
library(broom)
library(texreg)
library(MuMIn)

source("vars.R")
source("funs.R")

theme_set(theme_bw(base_size = 24))

# load data
load("./data/edit/allbus.R")
load("./data/edit/refugee.R")

# specify paths
tables_path <- "output/tables/supp/r_spec_curve/"
plots_path <- "output/plots/supp/r_spec_curve/"
models_path <- "output/models/supp/r_spec_curve/"

# specify models
analysis_a <- allbus %>%
    filter(sample_35 == 1)
analysis_r <- refugee %>%
    filter(sample_35 == 1)

ivs_allbus <- paste0("~ treat + ", paste(vars_allbus_z, collapse = " + "))
ivs_refugee <- paste0("~ treat + ", paste(vars_refugee_z, collapse = " + "))

# Run models #####

full_allbus <- lm(formula(paste("angry_asyl_bin", ivs_allbus)),
    data = analysis_a, na.action = "na.fail"
)

full_refugee <- lm(formula(paste("discrim_bin", ivs_refugee)),
    data = analysis_r, na.action = "na.fail"
)

# get complete list of models as separate strings
all_allbus <- dredge(full_allbus, fixed = c("treat"), eval = F) %>%
    lapply(toString) %>%
    str_remove_all("lm, angry_asyl_bin") %>%
    str_remove_all(", analysis_a, na.fail")

all_refugee <- dredge(full_refugee, fixed = c("treat"), eval = F) %>%
    lapply(toString) %>%
    str_remove_all("lm, discrim_bin") %>%
    str_remove_all(", analysis_r, na.fail")


# dependent variables #####
dvs_emo <- c(
    "z_emo_asyl_pca",
    "scared_asyl_bin", "angry_asyl_bin", "pity_asyl_bin", "sympa_asyl_bin",
    "scared_ital_bin", "angry_ital_bin", "pity_ital_bin", "sympa_ital_bin",
    "scared_turk_bin", "angry_turk_bin", "pity_turk_bin", "sympa_turk_bin",
    "scared_pole_bin", "angry_pole_bin", "pity_pole_bin", "sympa_pole_bin",
    "scared_jew_bin", "angry_jew_bin", "pity_jew_bin", "sympa_jew_bin"
)
dvs_risk <- c(
    "z_asyl_risk_pca",
    "asyl_risk_safety_bin", "asyl_risk_cohesion_bin",
    "asyl_risk_state_bin", "asyl_risk_economy_bin"
)
dvs_dist <- c(
    "z_neighbor_asyl", "z_neighbor_ital", "z_neighbor_pole",
    "z_neighbor_turk", "z_neighbor_jew",
    "z_diff_asyl", "z_diff_ital", "z_diff_pole", "z_diff_turk", "z_diff_jew"
)
dvs_host <-  c("discrim_bin", "worries_antiimmig_bin", "z_change_in_welcome")
dvs_mhealth <-  c( "z_phq_4", "z_mcs")


# Run Models
dvs <- c(dvs_allbus_z, dvs_refugee_z)

full_models <- c()

for (i in dvs) {
    print(paste("Generating all possible models for:", i, "..."))

    if (i %in% dvs_allbus_z) {
        fit_allbus <- lapply(all_allbus,
            FUN = function(x) {
                lm_robust(formula(paste(i, x)),
                    data = analysis_a,
                    subset = sample_29 == 1,
                    se_type = "HC1"
                )
            }
        )
        names(fit_allbus) <- paste0(all_allbus)

        for (model in names(fit_allbus)) {
            tidy_temp <- tidy(fit_allbus[[model]]) %>%
                mutate(
                    nobs = fit_allbus[[model]]$nobs,
                    model = model,
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
    } else if (i %in% dvs_refugee_z) {
        fit_refugee <- lapply(all_refugee,
            FUN = function(x) {
                lm_robust(formula(paste(i, x)),
                    data = analysis_r,
                    subset = sample_29 == 1,
                    se_type = "HC1"
                )
            }
        )
        names(fit_refugee) <- paste0(all_refugee)

        for (model in names(fit_refugee)) {
            tidy_temp <- tidy(fit_refugee[[model]]) %>%
                mutate(
                    nobs = fit_refugee[[model]]$nobs,
                    model = model,
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
}

full_models <- full_models %>%
    mutate(
        "Sex" = ifelse(str_detect(model, "female"), 1, 0),
        "Education" = ifelse(str_detect(model, "edu_tertiary"), 1, 0),
        "Age" = ifelse(str_detect(model, "z_age"), 1, 0),
        "Year" = ifelse(str_detect(model, "year_s"), 1, 0),
        "East Germany" = ifelse(str_detect(model, "east"), 1, 0),
        "Int. contacts" = ifelse(str_detect(model, "z_intk"), 1, 0),
        "Married" = ifelse(str_detect(model, "married"), 1, 0),
        "Unemployed" = ifelse(str_detect(model, "ability:class_year:ses"), 1, 0),
        "Refugee" = ifelse(str_detect(model, "is_refugee"), 1, 0),
        "Syrian" = ifelse(str_detect(model, "cit_syria_bin"), 1, 0),
        "Iraqi" = ifelse(str_detect(model, "cit_iraq_bin"), 1, 0),
        "Afghan" = ifelse(str_detect(model, "cit_afghan_bin"), 1, 0),
        "Comm. accomm." = ifelse(str_detect(model, "comm_accom"), 1, 0)
    )

full_models <- full_models %>%
  filter(str_detect(term, "treat")) %>%
  group_by(outcome) %>%
  arrange(estimate) %>%
  mutate(h_order = 1:n())

save(full_models,
    file = paste0(models_path, "full_models.Rda")
)
