# ---
# Modifying variables
# ---

library(tidyverse)

source("./vars.R")

tables_path <- "output/tables/desc/"
plots_path <- "output/plots/desc/"
models_path <- "output/models/desc/"

theme_set(theme_bw(base_size = 24))

# Generate factor variables #####

# Allbus #####

# Emotions

# select vars for PCA
emo_vars_pca <- allbus %>%
  select(starts_with(c("angry_asyl", "scared_asyl", "pity_asyl", "sympa_asyl"))) %>%
  mutate(
    sympa_asyl = car::recode(sympa_asyl, "1 = 4; 2 = 3; 3 = 2; 4 = 1"),
    pity_asyl = car::recode(pity_asyl, "1 = 4; 2 = 3; 3 = 2; 4 = 1")
  ) %>%
  select(!ends_with("_bin"))

# Run PCA
emo_pca <- prcomp(emo_vars_pca, center = TRUE, scale. = TRUE)

# Plot amount of variance explained by each PC
var_explained <- emo_pca$sdev^2 / sum(emo_pca$sdev^2) # PC1 explains 58% of the total variance

g1 <- data.frame(var_explained, PC = c("PC1", "PC2", "PC3", "PC4")) %>%
  ggplot(aes(y = var_explained, x = PC, group = 1)) +
  geom_point() +
  geom_line() +
  ylim(c(0, 0.7)) +
  ylab("Variance explained (%)") +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_line(size = 0.25)
  )

ggsave(g1, file = paste0(plots_path, "emo_pca.pdf"), width = 6, height = 4)
print(g1) 

# Combine with main data

allbus <- allbus %>%
  mutate(emo_asyl_pca = emo_pca$x[, 1])


# Risk assessments

# select vars for PCA
risk_vars_pca <- allbus %>%
  select(starts_with(c("asyl_risk_"))) %>%
  select(!ends_with("_bin")) %>%
  select(!ends_with("pca"))
  

# run PCA
risk_pca <- prcomp(risk_vars_pca, center = TRUE, scale. = TRUE)


# Plot amount of variance explained by each PC
var_explained <- risk_pca$sdev^2 / sum(risk_pca$sdev^2) # PC1 explains 66% of the total variance


g1 <- data.frame(var_explained, PC = c("PC1", "PC2", "PC3", "PC4")) %>%
  ggplot(aes(y = var_explained, x = PC, group = 1)) +
  geom_point() +
  geom_line() +
  ylim(c(0, 0.7)) +
  ylab("Variance explained (%)") +
  theme(axis.title.x = element_blank(),
        panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_line(size = 0.25)
  )
ggsave(g1, file = paste0(plots_path, "risk_pca.pdf"), width = 6, height = 4)
print(g1)

# Combine with main data
allbus <- allbus %>%
  mutate(asyl_risk_pca = -risk_pca$x[, 1])

# Standardize data #####

# Note: Standardising for 2*sd(control_group)

# Allbus #####

# obtain SDs for all numeric variables of control group
allbus_control_sd <- allbus %>%
  filter(treat == 0) %>%
  filter(sample_29 == 1) %>%
  select_if(is.numeric) %>%
  select(-ends_with("_bin")) %>%
  mutate_all(sd, na.rm = T) %>%
  unique(.)

# only select variables you are interested in
z_allbus <- allbus %>%
  select(
    starts_with("cit"), starts_with("emo"), starts_with("diff_"), starts_with("neighbor_"),
    starts_with("asyl_risk"),
    starts_with("for_"),
    starts_with("contact_for"),
    starts_with("for_discrim"),
    starts_with("att_for_rights"),
    starts_with("feeling"),
    starts_with("att_islam"),
    all_of(vars_allbus),
    "health"
  ) %>%
  select(-ends_with("_bin")) %>%
  select(-c("female", "east", "edu_tertiary", "married", "unemp"))

# standardize variables
for (i in names(z_allbus)) {
  z_allbus[i] <- (z_allbus[[i]] - mean(z_allbus[[i]], na.rm = T)) / (2 * allbus_control_sd[[i]])
}

# change names and merge with main data
z_allbus <- z_allbus %>%
  setNames(paste0("z_", names(.)))

allbus <- bind_cols(allbus, z_allbus)

# Soep --------------------
# obtain SDs for all numeric variables of control group
refugee_control_sd <- refugee %>%
  filter(treat == 0) %>%
  filter(sample_29 == 1) %>%
  select_if(is.numeric) %>%
  select(-ends_with("_bin")) %>%
  mutate_all(sd, na.rm = T) %>%
  unique(.)

# select numeric DVs and IVs
z_refugee <- refugee %>%
  select(all_of(c(dvs_refugee, vars_refugee))) %>%
  select(-ends_with("_bin")) %>%
  select(-c("female", "east", "is_refugee", "comm_accom"))

# standardize variables
for (i in names(z_refugee)) {
  z_refugee[i] <- (z_refugee[[i]] - mean(z_refugee[[i]], na.rm = T)) / (2 * refugee_control_sd[[i]])
}

# change names and merge with main data
z_refugee <- z_refugee %>%
  setNames(paste0("z_", names(.)))

refugee <- bind_cols(refugee, z_refugee)



rm(z_allbus, z_refugee, allbus_control_sd, refugee_control_sd)
