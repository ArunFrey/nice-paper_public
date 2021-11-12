# ---
# Supplementary Analysis: Effects by east and west Germany
# ---

library(tidyverse)
library(estimatr)
library(broom)
library(texreg)

source("./src/vars.R")

theme_set(theme_bw(base_size = 24))

# load data
load("./data/edit/allbus.R")
load("./data/edit/refugee.R")

# specify paths
tables_path <- "output/tables/supp/r_east_west/"
plots_path <- "output/plots/supp/r_east_west/"
models_path <- "output/models/supp/r_east_west/"

# specify table suffix 
tab_suffix_east <- "East Germany"
lab_suffix_east <- "_east"
tab_suffix_west <- "West Germany"
lab_suffix_west <- "_west"

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
    "z_emo_asyl_pca",
    "scared_asyl_bin", "angry_asyl_bin", "pity_asyl_bin", "sympa_asyl_bin",
    "scared_ital_bin", "angry_ital_bin", "pity_ital_bin", "sympa_ital_bin",
    "scared_turk_bin", "angry_turk_bin", "pity_turk_bin", "sympa_turk_bin",
    "scared_pole_bin", "angry_pole_bin", "pity_pole_bin", "sympa_pole_bin",
    "scared_jew_bin", "angry_jew_bin", "pity_jew_bin", "sympa_jew_bin"
)

dvs_emo_asyl <- dvs_emo[str_detect(dvs_emo, "asyl")]

# run model
fit_emo_east <- lapply(dvs_emo,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = analysis_a[analysis_a$east==1, ],
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)
names(fit_emo_east) <- dvs_emo

fit_emo_west <- lapply(dvs_emo,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = analysis_a[analysis_a$east==0, ],
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)
names(fit_emo_west) <- dvs_emo

# Risks #####
dvs_risk <- c(
    "z_asyl_risk_pca",
    "asyl_risk_safety_bin", "asyl_risk_cohesion_bin",
    "asyl_risk_state_bin", "asyl_risk_economy_bin"
)

# run model
fit_risk_east <- lapply(dvs_risk,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = analysis_a[analysis_a$east==1, ],
            subset = sample_29 == 1,
                    se_type = "HC1"

        )
    }
)
names(fit_risk_east) <- dvs_risk

fit_risk_west <- lapply(dvs_risk,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = analysis_a[analysis_a$east==0, ],
            subset = sample_29 == 1,
                    se_type = "HC1"

        )
    }
)
names(fit_risk_west) <- dvs_risk

# Social Distance #####
dvs_dist <- c(
    "z_neighbor_asyl", "z_neighbor_ital", "z_neighbor_pole",
    "z_neighbor_turk", "z_neighbor_jew",
    "z_diff_asyl", "z_diff_ital", "z_diff_pole", "z_diff_turk", "z_diff_jew"
)

dvs_dist_asyl <- dvs_dist[str_detect(dvs_dist, "asyl")]

# Run model
fit_dist_east <- lapply(dvs_dist,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = analysis_a[analysis_a$east==1, ],
            subset = sample_29 == 1,
                        se_type = "HC1"

        )
    }
)
names(fit_dist_east) <- dvs_dist

fit_dist_west <- lapply(dvs_dist,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = analysis_a[analysis_a$east==1, ],
            subset = sample_29 == 1,
                        se_type = "HC1"

        )
    }
)
names(fit_dist_west) <- dvs_dist


# REFUGEE #####

# specify model
ivs_refugee <- paste0("~ treat + ", paste0(vars_refugee_z, collapse = " + "))

# Hostility #####

dvs_host <-  c("discrim_bin", "worries_antiimmig_bin", "z_change_in_welcome")

# run model
fit_host_east <- lapply(dvs_host,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_refugee)),
            data = analysis_r[analysis_r$east==1, ],
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)
names(fit_host_east) <- dvs_host

fit_host_west <- lapply(dvs_host,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_refugee)),
            data = analysis_r[analysis_r$east==0, ],
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)
names(fit_host_west) <- dvs_host

# Mental Health #####

dvs_mhealth <-  c( "z_phq_4", "z_mcs")

# run model
fit_mhealth_east <- lapply(dvs_mhealth,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_refugee)),
            data = analysis_r[analysis_r$east == 1, ],
            subset = sample_29 == 1,
            se_type = "HC1"
        )
    }
)
names(fit_mhealth_east) <- dvs_mhealth

fit_mhealth_west <- lapply(dvs_mhealth,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_refugee)),
            data = analysis_r[analysis_r$east == 0, ],
            subset = sample_29 == 1,
            se_type = "HC1"
        )
    }
)
names(fit_mhealth_west) <- dvs_mhealth


# Save tables #####

# ALLBUS #####

# Emotions #####

# Save Tables #####
print(
    screenreg(fit_emo_east[dvs_emo_asyl],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Factor", "Fear", "Anger", "Pity", "Affection"),
        custom.coef.map = labs
    )
)

texreg(fit_emo_east[dvs_emo_asyl],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Factor", "Fear", "Anger", "Pity", "Affection"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on feelings towards asylum seekers", tab_suffix_east, sep = ", "),
    label = paste0("tab_emo", lab_suffix_east),
    center = TRUE,
    file = paste0(tables_path, "tab_emo_east.tex")
)

print(
    screenreg(fit_emo_west[dvs_emo_asyl],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Factor", "Fear", "Anger", "Pity", "Affection"),
        custom.coef.map = labs
    )
)

texreg(fit_emo_west[dvs_emo_asyl],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Factor", "Fear", "Anger", "Pity", "Affection"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on feelings towards asylum seekers", tab_suffix_west, sep = ", "),
    label = paste0("tab_emo", lab_suffix_west),
    center = TRUE,
    file = paste0(tables_path, "tab_emo_west.tex")
)


# Risk #####
print(
    screenreg(fit_risk_east[dvs_risk],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Factor", "Safety", "Cohesion", "Welfare State", "Economy"),
        custom.coef.map = labs
    )
)

texreg(fit_risk_east[dvs_risk],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Factor", "Safety", "Cohesion", "Welfare State", "Economy"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on risks associated with asylum seekers", tab_suffix_east, sep = ", "),
    label = paste0("tab_risk", lab_suffix_east),
    center = TRUE,
    file = paste0(tables_path, "tab_risk_east.tex")
)

print(
    screenreg(fit_risk_west[dvs_risk],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Factor", "Safety", "Cohesion", "Welfare State", "Economy"),
        custom.coef.map = labs
    )
)

texreg(fit_risk_west[dvs_risk],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Factor", "Safety", "Cohesion", "Welfare State", "Economy"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on risks associated with asylum seekers", tab_suffix_west, sep = ", "),
    label = paste0("tab_risk", lab_suffix_west),
    center = TRUE,
    file = paste0(tables_path, "tab_risk_west.tex")
)

# Social Distance #####
print(
    screenreg(fit_dist_east[dvs_dist_asyl],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Refugee as Neighbour", "Difference to Germans"),
        custom.coef.map = labs
    )
)

texreg(fit_dist_east[dvs_dist_asyl],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Refugee as Neighbour", "Difference to Germans"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on perceived social distance", tab_suffix_east, sep = ", "),
    label = paste0("tab_dist", lab_suffix_east),
    center = TRUE,
    file = paste0(tables_path, "tab_dist_east.tex")
)

print(
    screenreg(fit_dist_west[dvs_dist_asyl],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Refugee as Neighbour", "Difference to Germans"),
        custom.coef.map = labs
    )
)

texreg(fit_dist_west[dvs_dist_asyl],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Refugee as Neighbour", "Difference to Germans"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on perceived social distance", tab_suffix_west, sep = ", "),
    label = paste0("tab_dist", lab_suffix_west),
    center = TRUE,
    file = paste0(tables_path, "tab_dist_west.tex")
)

# REFUGEE #####

# Hostility #####
print(
    screenreg(fit_host_east[dvs_host],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Discrimination", "Anti-immig. worries", "Change in welcome"),
        custom.coef.map = labs
    )
)

texreg(fit_host_east[dvs_host],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Discrimination", "Anti-immig. worries", "Change in welcome"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on refugees and asylum seekers", tab_suffix_east, sep = ", "),
    label = paste0("tab_host", lab_suffix_east),
    center = TRUE,
    file = paste0(tables_path, "tab_host_east.tex")
)

print(
    screenreg(fit_host_west[dvs_host],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Discrimination", "Anti-immig. worries", "Change in welcome"),
        custom.coef.map = labs
    )
)

texreg(fit_host_west[dvs_host],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("Discrimination", "Anti-immig. worries", "Change in welcome"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on refugees and asylum seekers", tab_suffix_west, sep = ", "),
    label = paste0("tab_host", lab_suffix_west),
    center = TRUE,
    file = paste0(tables_path, "tab_host_west.tex")
)

# Mental health #####
print(
    screenreg(fit_mhealth_east[dvs_mhealth],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Mental distress", "Mental health"),
        custom.coef.map = labs
    )
)

texreg(fit_mhealth_east[dvs_mhealth],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
        custom.model.names = c("Mental distress", "Mental health"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on refugees' and asylum seekers' well-being", tab_suffix_east, sep = ", "),
    label = paste0("tab_mhealth", lab_suffix_east),
    center = TRUE,
    file = paste0(tables_path, "tab_mhealth_east.tex")
)

print(
    screenreg(fit_mhealth_west[dvs_mhealth],
        include.ci = FALSE,
        stars = c(0.01, 0.05, 0.1),
        custom.model.names = c("Mental distress", "Mental health"),
        custom.coef.map = labs
    )
)

texreg(fit_mhealth_west[dvs_mhealth],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
        custom.model.names = c("Mental distress", "Mental health"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = paste("Impact of July attacks on refugees' and asylum seekers' well-being", tab_suffix_west, sep = ", "),
    label = paste0("tab_mhealth", lab_suffix_west),
    center = TRUE,
    file = paste0(tables_path, "tab_mhealth_west.tex")
)

# Save models #####
fit_east <- c(
    fit_emo_east, fit_risk_east,
    fit_dist_east,fit_host_east, 
    fit_mhealth_east
)

fit_west <- c(
    fit_emo_west,  fit_risk_west,
    fit_dist_west, fit_host_west, 
    fit_mhealth_west
)

full_models_east <- c()
full_models_west <- c()


for (i in names(fit_east)) {
    tidy_temp_e <- tidy(fit_east[[i]]) %>%
        mutate(
            nobs = fit_east[[i]]$nobs, 
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
            ),
            region = "East"
        )
        
    tidy_temp_w <- tidy(fit_west[[i]]) %>%
        mutate(
            nobs = fit_west[[i]]$nobs,
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
            ),
            region = "West"
        )
    full_models_east <- rbind(full_models_east, tidy_temp_e)
    full_models_west <- rbind(full_models_west, tidy_temp_w)
}

full_models <- rbind(full_models_east, full_models_west)

# save models
saveRDS(full_models, file = paste0(models_path, "models.rds"))
