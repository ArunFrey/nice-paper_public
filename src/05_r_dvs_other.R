# ---
# Main Analysis
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
tables_path <- "output/tables/supp/r_dvs_other/"
plots_path <- "output/plots/supp/r_dvs_other/"
models_path <- "output/models/supp/r_dvs_other/"


# specify models
analysis_a <- allbus %>%
    mutate(pride_nation = ifelse(pn11 %in% c(1, 2), 1,
                                 ifelse(is.na(pn11), NA, 0)), 
           close_country = ifelse(pn16 %in% c(1, 2), 1,
                                  ifelse(is.na(pn16), NA, 0)),
           close_eu = ifelse(pn17 %in% c(1, 2), 1,
                             ifelse(is.na(pn17), NA, 0)),
    ) %>%
    filter(sample_35 == 1)

# Run models #####

# Allbus #####

# Specify model
ivs_allbus <- paste0("~ treat + ", paste(vars_allbus_z, collapse = " + "))

# Immigration of asylum seekers #####
dvs_immi <- c("immi_asyl_war_bin", "immi_asyl_pers_bin", "immi_asyl_econ_bin") 

# run model
fit_immi <- lapply(dvs_immi,
    FUN = function(x) {
        estimatr::lm_robust(formula(paste(x, ivs_allbus)),
            data = allbus,
            subset = sample_29 == 1, 
            se_type = "HC1"
        )
    }
)

names(fit_immi) <- dvs_immi


# Subjective health #####
dvs_health <- c("z_health") 

# run model
fit_health <- lapply(dvs_health,
                   FUN = function(x) {
                       estimatr::lm_robust(formula(paste(x, ivs_allbus)),
                                           data = allbus,
                                           subset = sample_29 == 1, 
                                           se_type = "HC1"
                       )
                   }
)

names(fit_health) <- dvs_health

## Solidarity/National pride
dvs_soli <- c("close_country", "close_eu")

# run model
fit_soli <- lapply(dvs_soli,
                   FUN = function(x) {
                       estimatr::lm_robust(formula(paste(x, ivs_allbus)),
                                           data = analysis_a,
                                           subset = sample_29 == 1, 
                                           se_type = "HC1"
                       )
                   }
)

names(fit_soli) <- dvs_soli



# Effect over time #####

# Health
dvs_time <- c("z_health")

# run model
fit_time <- c()

for(i in c(5:35)) {
    sample <- paste0("sample2_", i)
    
    fit_temp <- lapply(dvs_time,
                       FUN = function(x) {
                           estimatr::lm_robust(formula(paste(x, ivs_allbus)),
                                               data = analysis_a,
                                               subset = get(sample) == 1,
                                               se_type = "HC1"
                           )
                       }
    )
    names(fit_temp) <- paste(dvs_time, i, sep = ".")
    fit_time <- c(fit_time, fit_temp)
}

# Save tables #####

texreg(fit_immi[dvs_immi],
    digits = 2,
    include.ci = FALSE,
    stars = c(0.01, 0.05, 0.1),
    booktabs = TRUE,
    custom.model.names = c("War", "Political persecution", "Economic reasons"),
    custom.coef.map = labs,
    caption.above = TRUE,
    use.packages = FALSE,
    caption = "Restrict the immigration of refugees who are fleeing because of...",
    label = "tab_immi",
    center = TRUE,
    file = paste0(tables_path, "tab_immi.tex")
)

texreg(fit_health[dvs_health],
       digits = 2,
       include.ci = FALSE,
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       custom.model.names = c("Subj. health"),
       custom.coef.map = labs,
       caption.above = TRUE,
       use.packages = FALSE,
       caption = "Impact of July 2016 terrorist attacks on subjective health assessments of German respondents",
       label = "tab_health",
       center = TRUE,
       file = paste0(tables_path, "tab_health.tex")
)

texreg(fit_soli, 
       digits = 2,
       include.ci = FALSE,
       stars = c(0.01, 0.05, 0.1),
       booktabs = TRUE,
       custom.model.names = c( "Connectedness (country)", "Connectedness (EU)"),
       custom.coef.map = labs,
       caption.above = TRUE,
       use.packages = FALSE,
       caption = paste("Impact of July 2016 terrorist attacks on Germans' sense of solidarity and national pride"),
       label = paste0("tab_soli"),
       center = TRUE,
       file = paste0(tables_path, "tab_soli.tex")
)

# Save models #####
full_models <- models_time <- c()

fit <- c(fit_immi, fit_health, fit_soli)

for (i in names(fit)) {
    tidy_temp <- tidy(fit[[i]]) %>%
        mutate(
            nobs = fit[[i]]$nobs,
            group = ifelse(outcome %in% dvs_immi, "immi",
                           ifelse(outcome %in% dvs_health, "health",
                                  ifelse(outcome %in% dvs_soli, "soli", NA)
        )))
    full_models <- rbind(full_models, tidy_temp)
}

for (i in names(fit_time)) {
    ndays <- sub(".*\\.", "", i)
    
    tidy_temp <- tidy(fit_time[[i]]) %>%
        mutate(
            nobs = fit[[i]]$nobs,
            days = as.numeric(ndays),
            group =  "health"
        )
    models_time <- rbind(models_time, tidy_temp)
}

# save models
saveRDS(full_models, file = paste0(models_path, "models.rds"))
saveRDS(models_time, file = paste0(models_path, "models_time.rds"))
