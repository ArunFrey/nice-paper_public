# ---
# Main Analysis
# ---

library(tidyverse)
library(estimatr)
library(broom)
library(texreg)
library(cobalt)
library(WeightIt)
library(patchwork)

source("./src/vars.R")

theme_set(theme_bw(base_size = 24))

# load data
load("./data/edit/allbus.R")
load("./data/edit/refugee.R")

# specify paths
tables_path <- "output/tables/supp/r_matching/"
plots_path <- "output/plots/supp/r_matching/"
models_path <- "output/models/supp/r_matching/"

# specify analysis data
analysis_a <- allbus %>%
    filter(sample_35==1)
analysis_r <- refugee %>%
    filter(sample_35==1)

# Matching #####

 # Including interview contacts for balance plot
vars_match <- vars_allbus_z
fun_allbus <- formula(paste0("treat ~ ", paste(vars_match, collapse = " + ")))

vars_match <- vars_refugee_z
fun_refugee <- formula(paste0("treat ~ ", paste(vars_match, collapse = " + ")))


# ENTROPY BALANCING (matching on mean, variance, and skew)
m_ebal_allbus <- weightit(fun_allbus, data = analysis_a, int = TRUE,
                   method = "ebal", moments = 3, replace = T, estimand = "ATT")
m_ebal_refugee <- weightit(fun_refugee, data = analysis_r, int = TRUE,
                   method = "ebal", moments = 3, replace = T, estimand = "ATT")

# CHECK BALANCE
set.cobalt.options(binary = "std")

## Using alternate variable names
g1 <- love.plot(m_ebal_allbus, threshold = .1, abs = T, binary = "std",
                var.order = "unadjusted", var.names = labs, size = 4,
                title = "German sample",
                shapes = c("circle filled", "triangle filled"),
                colors = c("black", "black"), 
                limits = list(m = c(0, 0.51))) +
  theme_bw(base_size = 20)

## Using alternate variable names
g2 <- love.plot(m_ebal_refugee, threshold = .1, abs = T, binary = "std",
                var.order = "unadjusted", var.names = labs, size = 4,
                title = "Refugee sample",
                shapes = c("circle filled", "triangle filled"),
                colors = c("black", "black"), 
                limits = list(m = c(0, 0.51))) +
  theme_bw(base_size = 20)

g1 + g2 + plot_layout(guides = "collect")
ggsave(last_plot(), file = paste0(plots_path, "bal_plot.pdf"), width = 13, height = 6)


# Excluding interview contacts for analysis
vars_match <- vars_allbus_z[!vars_allbus_z %in% "z_intk"]
fun_allbus <- formula(paste0("treat ~ ", paste(vars_match, collapse = " + ")))

vars_match <- vars_refugee_z[!vars_refugee_z %in% "z_intk"]
fun_refugee <- formula(paste0("treat ~ ", paste(vars_match, collapse = " + ")))

m_ebal_allbus <- weightit(fun_allbus, data = analysis_a, int = TRUE,
                          method = "ebal", moments = 3, replace = T, estimand = "ATT")
m_ebal_refugee <- weightit(fun_refugee, data = analysis_r, int = TRUE,
                           method = "ebal", moments = 3, replace = T, estimand = "ATT")

# save weights #####

analysis_a <- analysis_a %>%
  mutate(w_ebal = get.w(m_ebal_allbus))

analysis_r <- analysis_r %>%
  mutate(w_ebal = get.w(m_ebal_refugee))

# Run models #####

# Allbus #####

# Specify model
ivs_allbus <- paste0("~ treat")

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
            data = analysis_a,
            subset = sample_29 == 1, 
            se_type = "HC1",
            weights = w_ebal
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
            data = analysis_a,
            subset = sample_29 == 1,
            se_type = "HC1",
            weights = w_ebal
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
            data = analysis_a,
            subset = sample_29 == 1,
            se_type = "HC1",
            weights = w_ebal
        )
    }
)

names(fit_dist) <- dvs_dist


# REFUGEE #####

# specify model
ivs_refugee <- paste0("~ treat")

# Hostility #####

dvs_host <-  c("discrim_bin", "worries_antiimmig_bin", "z_change_in_welcome")

# run model
fit_host <- lapply(dvs_host,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_refugee)),
            data = analysis_r,
            subset = sample_29 == 1, 
            se_type = "HC1",
            weights = w_ebal
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
            data = analysis_r,
            subset = sample_29 == 1, 
            se_type = "HC1",
            weights = w_ebal
        )
    }
)

names(fit_mhealth) <- dvs_mhealth


# Effect over time #####

# ALLBUS
dvs_time <- c(
    "scared_asyl_bin", "angry_asyl_bin", "pity_asyl_bin", "sympa_asyl_bin",
    "asyl_risk_safety_bin", "asyl_risk_cohesion_bin", "asyl_risk_state_bin", "asyl_risk_economy_bin",
    "z_neighbor_asyl", "z_diff_asyl", "z_health"
)

# run model
fit_time <- c()

for(i in c(5:35)) {
    sample <- paste0("sample2_", i)
    
    fit_temp <- lapply(dvs_time,
                       FUN = function(x) {
                           estimatr::lm_robust(formula(paste(x, ivs_allbus)),
                                               data = analysis_a,
                                               subset = get(sample) == 1,
                                               se_type = "HC1",
                                               weights = w_ebal
                           )
                       }
    )
    names(fit_temp) <- paste(dvs_time, i, sep = ".")
    fit_time <- c(fit_time, fit_temp)
}


# REFUGEES
dvs_time <-  c("discrim_bin", "worries_antiimmig_bin", "z_change_in_welcome", "z_phq_4", "z_mcs")

# run model

for(i in c(5:35)) {
    sample <- paste0("sample2_", i)
    
    fit_temp <- lapply(dvs_time,
        FUN = function(x) {
            estimatr::lm_robust(formula(paste(x, ivs_refugee)),
                data = analysis_r,
                subset = get(sample) == 1,
                se_type = "HC1",
                weights = w_ebal
            )
        }
    )
    names(fit_temp) <- paste(dvs_time, i, sep = ".")
    fit_time <- c(fit_time, fit_temp)
}


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
    caption = "Impact of July attacks on feelings towards asylum seekers, entropy balanced",
    label = "tab_emo_match",
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
    caption = "Impact of July attacks on risks associated with asylum seekers, entropy balanced",
    label = "tab_risk_match",
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
    caption = "Impact of July attacks on perceived social distance, entropy balanced",
    label = "tab_dist_match",
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
    caption = "Impact of July attacks on refugees and asylum seekers, entropy balanced",
    label = "tab_host_match",
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
    caption = "Impact of July attacks on refugees' and asylum seekers' well-being, entropy balanced",
    label = "tab_mhealth_match",
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

models_time <- c()

for (i in names(fit_time)) {
    ndays <- sub(".*\\.", "", i)

    tidy_temp <- tidy(fit_time[[i]]) %>%
        mutate(
            nobs = fit[[i]]$nobs,
            days = as.numeric(ndays),
                        group = ifelse(
                outcome %in% dvs_emo, "emo",
                ifelse(
                    outcome %in% dvs_risk, "risk", 
                    ifelse(
                        outcome %in% dvs_dist, "dist",
                        ifelse(
                            outcome %in% dvs_host, "host",
                            ifelse(
                                outcome %in% dvs_mhealth, "mhealth", 
                                ifelse(
                                    outcome %in% c("health", "z_health"), "health", NA
                                )
                            )
                        )
                    )
                )
            )
        )
    models_time <- rbind(models_time, tidy_temp)
}


# save models
saveRDS(full_models, file = paste0(models_path, "models.rds"))
saveRDS(models_time, file = paste0(models_path, "models_time.rds"))
