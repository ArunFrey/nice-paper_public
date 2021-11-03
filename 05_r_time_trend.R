# ---
# Supplementary Analysis: Time trend
# ---

library(tidyverse)
library(estimatr)
library(broom)
library(texreg)

source("vars.R")
source("funs.R")

theme_set(theme_bw(base_size = 24))

# load data
load("./data/edit/allbus.R")
load("./data/edit/refugee.R")

# specify paths
tables_path <- "output/tables/supp/r_time_trend/"
plots_path <- "output/plots/supp/r_time_trend/"
models_path <- "output/models/supp/r_time_trend/"

# Run models #####

# Allbus #####

# Specify model
ivs_allbus <- paste0("~ treat + time + ", paste(vars_allbus_z, collapse = " + "))

# Emotions #####
dvs_emo <- c(
    "z_emo_asyl_pca",
    "scared_asyl_bin", "angry_asyl_bin", "pity_asyl_bin", "sympa_asyl_bin",
    "scared_ital_bin", "angry_ital_bin", "pity_ital_bin", "sympa_ital_bin",
    "scared_turk_bin", "angry_turk_bin", "pity_turk_bin", "sympa_turk_bin",
    "scared_pole_bin", "angry_pole_bin", "pity_pole_bin", "sympa_pole_bin",
    "scared_jew_bin", "angry_jew_bin", "pity_jew_bin", "sympa_jew_bin"
)

dvs_emo_asyl <- dvs_emo[str_detect(dvs_emo, "asyl")]

# run model
fit_emo <- lapply(dvs_emo,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = allbus,
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)

names(fit_emo) <- dvs_emo

# Risks #####
dvs_risk <- c(
    "z_asyl_risk_pca",
    "asyl_risk_safety_bin", "asyl_risk_cohesion_bin",
    "asyl_risk_state_bin", "asyl_risk_economy_bin"
)

# run model
fit_risk <- lapply(dvs_risk,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = allbus,
            subset = sample_29 == 1,
                    se_type = "HC1"

        )
    }
)

names(fit_risk) <- dvs_risk

# Social Distance #####
dvs_dist <- c(
    "z_neighbor_asyl", "z_neighbor_ital", "z_neighbor_pole",
    "z_neighbor_turk", "z_neighbor_jew",
    "z_diff_asyl", "z_diff_ital", "z_diff_pole", "z_diff_turk", "z_diff_jew"
)

dvs_dist_asyl <- dvs_dist[str_detect(dvs_dist, "asyl")]

# Run model
fit_dist <- lapply(dvs_dist,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = allbus,
            subset = sample_29 == 1,
                        se_type = "HC1"

        )
    }
)

names(fit_dist) <- dvs_dist


# REFUGEE #####

# specify model
ivs_refugee <- paste0("~ treat + time + ", paste0(vars_refugee_z, collapse = " + "))

# Hostility #####

dvs_host <-  c("discrim_bin", "worries_antiimmig_bin", "z_change_in_welcome")

# run model
fit_host <- lapply(dvs_host,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_refugee)),
            data = refugee,
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)

names(fit_host) <- dvs_host

# Mental Health #####

dvs_mhealth <-  c( "z_phq_4", "z_mcs")

# run model
fit_mhealth <- lapply(dvs_mhealth,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_refugee)),
            data = refugee,
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)

names(fit_mhealth) <- dvs_mhealth


# Save tables #####

# ALLBUS #####

# Emotions #####

# Save Tables #####
print(
    screenreg(fit_emo[dvs_emo_asyl],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Factor", "Fear", "Anger", "Pity", "Affection"),
        custom.coef.map = labs
    )
)

texreg(fit_emo[dvs_emo_asyl],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Factor", "Fear", "Anger", "Pity", "Affection"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = "Impact of July attacks on feelings towards asylum seekers",
    label = "tab_emo",
    center = TRUE,
    file = paste0(tables_path, "tab_emo.tex")
)


# Risk #####
print(
    screenreg(fit_risk[dvs_risk],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Factor", "Safety", "Cohesion", "Welfare State", "Economy"),
        custom.coef.map = labs
    )
)

texreg(fit_risk[dvs_risk],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Factor", "Safety", "Cohesion", "Welfare State", "Economy"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = "Impact of July attacks on risks associated with asylum seekers",
    label = "tab_risk",
    center = TRUE,
    file = paste0(tables_path, "tab_risk.tex")
)

# Social Distance #####
print(
    screenreg(fit_dist[dvs_dist_asyl],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Refugee as Neighbour", "Difference to Germans"),
        custom.coef.map = labs
    )
)

texreg(fit_dist[dvs_dist_asyl],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Refugee as Neighbour", "Difference to Germans"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = "Impact of July attacks on perceived social distance",
    label = "tab_dist",
    center = TRUE,
    file = paste0(tables_path, "tab_dist.tex")
)

# REFUGEE #####

# Hostility #####
print(
    screenreg(fit_host[dvs_host],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Discrimination", "Anti-immig. worries", "Change in welcome"),
        custom.coef.map = labs
    )
)

texreg(fit_host[dvs_host],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Discrimination", "Anti-immig. worries", "Change in welcome"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = "Impact of July attacks on refugees and asylum seekers",
    label = "tab_host",
    center = TRUE,
    file = paste0(tables_path, "tab_host.tex")
)

# Mental health #####
print(
    screenreg(fit_mhealth[dvs_mhealth],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Mental distress", "Mental health"),
        custom.coef.map = labs
    )
)

texreg(fit_mhealth[dvs_mhealth],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
        custom.model.names = c("Mental distress", "Mental health"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = "Impact of July attacks on refugees' and asylum seekers' well-being",
    label = "tab_mhealth",
    center = TRUE,
    file = paste0(tables_path, "tab_mhealth.tex")
)

# Save models #####
full_models <- c()

fit <- c(fit_emo, fit_risk, fit_dist, fit_host, fit_mhealth)

for (i in names(fit)) {
    tidy_temp <- tidy(fit[[i]]) %>%
        mutate(
            nobs = fit[[i]]$nobs, 
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

# save models
saveRDS(full_models, file = paste0(models_path, "models.rds"))
