# ---
# Supplementary Analysis: glm models
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
tables_path <- "output/tables/supp/r_logit/"
plots_path <- "output/plots/supp/r_logit/"
models_path <- "output/models/supp/r_logit/"

# specify table suffix 
tab_suffix <- "logistic regression"
lab_suffix <- "_glm"

# specify models
analysis_a <- allbus %>%
    filter(sample_35 == 1)
analysis_r <- refugee %>%
    filter(sample_35 == 1)


# Run models #####

# Allbus #####

# Specify model
ivs_allbus <- paste0("~ treat + ", paste(vars_allbus_z, collapse = " + "))

# Emotions #####
dvs_emo <- c(
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
        glm(formula(paste(x, ivs_allbus)),
            data = analysis_a,
            subset = sample_29==1,
            family = "binomial"
        )
    }
)

names(fit_emo) <- dvs_emo

# Risks #####
dvs_risk <- c(
    "asyl_risk_safety_bin", "asyl_risk_cohesion_bin",
    "asyl_risk_state_bin", "asyl_risk_economy_bin"
)

# run model
fit_risk <- lapply(dvs_risk,
    FUN = function(x) {
        glm(formula(paste(x, ivs_allbus)),
            data = analysis_a,
            subset = sample_29==1,
            family = binomial
            )
    }
)

names(fit_risk) <- dvs_risk


# REFUGEE #####

# specify model
ivs_refugee <- paste0("~ treat + ", paste0(vars_refugee_z, collapse = " + "))

# Hostility #####

dvs_host <-  c("discrim_bin", "worries_antiimmig_bin")

# run model
fit_host <- lapply(dvs_host,
    FUN = function(x) {
        glm(formula(paste(x, ivs_refugee)),
            data = analysis_r,
            subset = sample_29==1,
            family = binomial
            )
    }
)

names(fit_host) <- dvs_host


# Save tables #####

# ALLBUS #####

# Emotions #####

# Save Tables #####
print(
    screenreg(fit_emo[dvs_emo_asyl],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Fear", "Anger", "Pity", "Affection"),
        custom.coef.map = labs
    )
)

texreg(fit_emo[dvs_emo_asyl],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Fear", "Anger", "Pity", "Affection"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on feelings towards asylum seekers", tab_suffix, sep = ", "),
    label = paste0("tab_emo", lab_suffix),
    center = TRUE,
    file = paste0(tables_path, "tab_emo.tex")
)


# Risk #####
print(
    screenreg(fit_risk[dvs_risk],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Safety", "Cohesion", "Welfare State", "Economy"),
        custom.coef.map = labs
    )
)

texreg(fit_risk[dvs_risk],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Safety", "Cohesion", "Welfare State", "Economy"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on risks associated with asylum seekers", tab_suffix, sep = ", "),
    label = paste0("tab_risk", lab_suffix),
    center = TRUE,
    file = paste0(tables_path, "tab_risk.tex")
)

# REFUGEE #####

# Hostility #####
print(
    screenreg(fit_host[dvs_host],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Discrimination", "Change in welcome"),
        custom.coef.map = labs
    )
)

texreg(fit_host[dvs_host],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Discrimination", "Change in welcome"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on refugees and asylum seekers", tab_suffix, sep = ", "),
    label = paste0("tab_host", lab_suffix),
    center = TRUE,
    file = paste0(tables_path, "tab_host.tex")
)

# Save models #####
full_models <- c()

fit <- c(fit_emo, fit_risk, fit_host)

for (i in names(fit)) {
    tidy_temp <- tidy(fit[[i]]) %>%
        mutate(
            nobs = fit[[i]]$nobs,
            outcome = i,
            group = ifelse(
                names(fit[i]) %in% dvs_emo, "emo",
                ifelse(
                    names(fit[i]) %in% dvs_risk, "risk",
                    ifelse(
                        names(fit[i]) %in% dvs_host, "host", NA
                    )
                )
            )
        )
    full_models <- rbind(full_models, tidy_temp)
}


# save models
saveRDS(full_models, file = paste0(models_path, "models.rds"))
