# ---
# Plotting results 
# ---

library(tidyverse)
library(scales)

# set theme
theme_set(theme_bw(base_size = 20))

# source variable names
source("vars.R")

# select vars of interest for time trend
vars_time <- c(
    "angry_asyl_bin", "scared_asyl_bin",
    "asyl_risk_safety_bin", "asyl_risk_cohesion_bin", 
    "z_diff_asyl", "z_neighbor_asyl",
    "discrim_bin", "z_change_in_welcome", "z_mcs", "z_phq_4"
  )

# functions #####

# load datasets
load_data <- function(models_name = "models.Rds", filter_term = "treat") {
  models <- readRDS(paste0(models_path, models_name)) %>%
    filter(str_detect(term, filter_term)) %>%
    mutate(
      outcome_group =
        ifelse(str_detect(outcome, "asyl"), "Refugees",
          ifelse(str_detect(outcome, "ital"), "Italians",
            ifelse(str_detect(outcome, "jew"), "Jews",
              ifelse(str_detect(outcome, "turk"), "Turks",
                ifelse(str_detect(outcome, "pole"), "Poles", NA)
              )
            )
          )
        ),
      outcome_group = factor(outcome_group, levels = c("Refugees", "Italians", "Jews", "Poles", "Turks"))
    )
    
  models <- models %>%
    left_join(
      gather(as_tibble(labs), "outcome", "label") %>%
        mutate(label = as_factor(label))
      )
  }

# plot model, static
plot_options <- list(
  geom_vline(xintercept = 0, color = "red", linetype = 2),
  geom_errorbar(aes(
    xmin = estimate - 1.645 * std.error,
    xmax = estimate + 1.645 * std.error
  ),
  colour = "black", size = 1, width = 0
  ),
  geom_errorbar(aes(
    xmin = estimate - 1.96 * std.error, 
    xmax = estimate + 1.96 * std.error
  ),
  colour = "black", width = 0
  ),
  geom_point(aes(fill = I(ifelse(p.value < 0.1, 'black', 'white'))),
   shape = 21, size = 3.5, colour = "black"),
  scale_x_continuous(breaks = pretty_breaks(n = 5)),
  scale_y_discrete(
  ),
  xlab("Treatment effect"),
  theme(
    legend.position = "none",
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25),
    axis.title.y = element_blank()
  ),
  coord_cartesian(xlim = c(-0.155, 0.155))
)

# plot model, dynamic
plot_time <- list(
  geom_hline(yintercept = 0, color = "red", linetype = 2),
  geom_errorbar(aes(
    ymin = estimate - 1.645 * std.error,
    ymax = estimate + 1.645 * std.error
  ),
  colour = "black", size = 1, width = 0
  ),
  geom_errorbar(aes(
    ymin = estimate - 1.96 * std.error, 
    ymax = estimate + 1.96 * std.error
  ),
  colour = "black", width = 0
  ),
  geom_point(aes(fill = I(ifelse(p.value < 0.1, 'black', 'white'))),
   shape = 21, size = 3.5, colour = "black"),
  scale_y_continuous(breaks = pretty_breaks(n = 7)),
  ylab("Treatment effect"),
  xlab("Number of days in treatment group"),
  coord_cartesian(ylim = c(-0.28, 0.26)),
  theme(
    legend.position = "none",
    panel.grid.major = element_line(size = 0.5),
    panel.grid.minor = element_line(size = 0.25)
      )
)

# Main plots #####
plots_path <- "output/plots/main/"
models_path <- "output/models/main/"

models <- load_data()
models_time <- load_data("models_time.Rds")

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
    
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 

  g2 <- models_time %>%
    filter(group == dv_group) %>%
    filter(outcome %in% vars_time) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = days, y = estimate)) +
    plot_time +
    facet_grid(label~.)
  
  if(dv_group %in% c("mhealth", "dist")) {
    g2 <- g2 + ylab("Treatment effect (in 2 sd)")
  }
  ggsave(g1,
    file = paste0(plots_path, dv_group, ".pdf"),
    width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))

    ggsave(g2,
           file = paste0(plots_path, dv_group, "_time.pdf"),
           width = 10, height = 3 + 
             length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
    print(g1)
    print(g2)
}


# Matching #####
plots_path <- "output/plots/supp/r_matching/"
models_path <- "output/models/supp/r_matching/"

models <- load_data()
models_time <- load_data("models_time.Rds")

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 
  
  g2 <- models_time %>%
    filter(group == dv_group) %>%
    filter(outcome %in% vars_time) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = days, y = estimate)) +
    plot_time +
    facet_grid(label~.)
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
  
  ggsave(g2,
         file = paste0(plots_path, dv_group, "_time.pdf"),
         width = 10, height = 3 + 
           length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
}


# No controls #####
plots_path <- "output/plots/supp/r_no_controls/"
models_path <- "output/models/supp/r_no_controls/"

models <- load_data()
models_time <- load_data("models_time.Rds")

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 
  
  g2 <- models_time %>%
    filter(group == dv_group) %>%
    filter(outcome %in% vars_time) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = days, y = estimate)) +
    plot_time +
    facet_grid(label~.)
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
  
  ggsave(g2,
         file = paste0(plots_path, dv_group, "_time.pdf"),
         width = 10, height = 3 + 
           length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
}


# Na - case wide deletion #####
plots_path <- "output/plots/supp/r_na/"
models_path <- "output/models/supp/r_na/"

models <- load_data()
models_time <- load_data("models_time.Rds")

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 
  
  g2 <- models_time %>%
    filter(group == dv_group) %>%
    filter(outcome %in% vars_time) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = days, y = estimate)) +
    plot_time +
    facet_grid(label~.)
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
  
  ggsave(g2,
         file = paste0(plots_path, dv_group, "_time.pdf"),
         width = 10, height = 3 + 
           length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
}


# Na-Group - case wide deletion by model group #####
plots_path <- "output/plots/supp/r_na_group/"
models_path <- "output/models/supp/r_na_group/"

models <- load_data()
models_time <- load_data("models_time.Rds")

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 
  
  g2 <- models_time %>%
    filter(group == dv_group) %>%
    filter(outcome %in% vars_time) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = days, y = estimate)) +
    plot_time +
    facet_grid(label~.)
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
  
  ggsave(g2,
         file = paste0(plots_path, dv_group, "_time.pdf"),
         width = 10, height = 3 + 
           length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
}


# Brexit #####
plots_path <- "output/plots/supp/r_brexit/"
models_path <- "output/models/supp/r_brexit/"

models <- load_data()

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 

  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
}






# Reachability #####
plots_path <- "output/plots/supp/r_reachability/"
models_path <- "output/models/supp/r_reachability/"

models <- load_data()
models_time <- load_data("models_time.Rds")

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 
  
  g2 <- models_time %>%
    filter(group == dv_group) %>%
    filter(outcome %in% vars_time) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = days, y = estimate)) +
    plot_time +
    facet_grid(label~.)
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
  
  ggsave(g2,
         file = paste0(plots_path, dv_group, "_time.pdf"),
         width = 10, height = 3 + 
           length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
}







# Time trend #####
plots_path <- "output/plots/supp/r_time_trend/"
models_path <- "output/models/supp/r_time_trend/"

models <- load_data() %>%
  filter(term=="treat")

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options +
    scale_x_continuous(breaks = pretty_breaks(n = 3)) +
    coord_cartesian(xlim = c(-0.25, 0.25))

  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
}

# Treat alt periods: 21 and 35 days #####
plots_path <- "output/plots/supp/r_treat_alt_periods/"
models_path <- "output/models/supp/r_treat_alt_periods/"

models <- load_data()

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options  +
    facet_wrap(~sample)
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 10, height = 2 + length(unique(models$label[models$group==dv_group])))
}



# Logit Models #####

plots_path <- "output/plots/supp/r_logit/"
models_path <- "output/models/supp/r_logit/"

models <- load_data()

for(dv_group in c("emo", "risk", "host")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(str_detect(outcome, "bin")) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    plot_options +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    coord_cartesian(xlim = c(-0.7, 0.7))
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
}


# Treatment, excluding Nice #####
plots_path <- "output/plots/supp/r_treat_no_nice/"
models_path <- "output/models/supp/r_treat_no_nice/"

models <- load_data()

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options 
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
}

# Other dependent variables: attitudes towards immigration, national pride/solidarity, and subjective health####
plots_path <- "output/plots/supp/r_dvs_other/"
models_path <- "output/models/supp/r_dvs_other/"

models <- load_data()
models_time <- load_data("models_time.Rds")

for(dv_group in c("immi", "health", "soli")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    geom_hline(yintercept = 4.5, color = "black", linetype = 1) +
    plot_options  +
    coord_cartesian(xlim = c(-0.2, 0.2))
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
  
  if(dv_group=="health") {
    g2 <- models_time %>%
      filter(group == dv_group) %>%
      filter(outcome_group %in% c(NA, "Refugees")) %>%
      ggplot(aes(x = days, y = estimate)) +
      plot_time +
      facet_grid(label~.)  +
      ylab("Treatment effect (in 2 sd)")
    
    ggsave(g2,
           file = paste0(plots_path, dv_group, "_time.pdf"),
           width = 10, height = 3.8 + 
             length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
  }
}


# Treatment, ordinal #####
plots_path <- "output/plots/supp/r_treat_ordinal/"
models_path <- "output/models/supp/r_treat_ordinal/"

models <- load_data() %>%
  mutate(term = factor(str_remove_all(term, "treat_ordinal"), levels = c("Nice", "Würzburg", "Ansbach")))

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)), 
               group = term, shape = term, color = term, xmax = conf.high, xmin = conf.low)) +
    geom_errorbar(position = position_dodge(width = 0.3), width = 0) +
    geom_point(position = position_dodge2(width = 0.3), size = 3) +
    geom_hline(yintercept = 4.5, linetype = 1) +
    geom_vline(xintercept = 0, linetype = 2, color = "red") +
    scale_y_discrete() +
    scale_color_grey(start = 0.7, end = 0.1) +
    labs(color = "Period", shape = "Period") +
    xlab("Treatment effect") +
    theme(axis.title.y = element_blank(),
          legend.position = "top") +
    coord_cartesian(xlim = c(-0.25, 0.25))
  plot_options 
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
}

#  Difference between East and West Germany ####

plots_path <- "output/plots/supp/r_east_west/"
models_path <- "output/models/supp/r_east_west/"

models <- load_data() 

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)), 
               group = region, color = region)) +
    geom_point(size = 3.5, position = position_dodge(width=0.5)) +
    geom_errorbar(aes(xmin = estimate - 1.645 * std.error, 
                      xmax = estimate + 1.645 * std.error), 
                  width = 0, size = 1, 
                  position = position_dodge(width=0.5)) +
    geom_errorbar(aes(xmin = estimate - 1.96 * std.error, 
                      xmax = estimate + 1.96 * std.error), 
                  width = 0, 
                  position = position_dodge(width=0.5)) +
    geom_vline(xintercept = 0, linetype = 2, color = "darkgrey") +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    scale_y_discrete() +
    scale_color_grey(start = 0.7, end = 0.1) +
    labs(color = "Region") +
    theme(legend.position = "top", 
          axis.title.y = element_blank(),
          panel.grid.major = element_line(size = 0.5), 
          panel.grid.minor = element_line(size = 0.25)) +
    xlab("Treatment effect") +
    coord_cartesian(xlim = c(-0.3, 0.3))
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 6.5, height = 2 + length(unique(models$label[models$group==dv_group])))
}


# Treatment by week #####

plots_path <- "output/plots/supp/r_treat_by_week/"
models_path <- "output/models/supp/r_treat_by_week/"

models <- load_data(filter_term = "weeks_") 

for(dv_group in c("emo", "risk", "dist", "host", "mhealth")) {
  
  g1 <- models %>%
  filter(group == dv_group) %>%
  filter(outcome %in% vars_time) %>%
  filter(outcome_group %in% c(NA, "Refugees")) %>%
    ggplot(aes(x = term, y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(
      ymin = estimate - 1.645 * std.error,
      ymax = estimate + 1.645 * std.error
    ),
    colour = "black", size = 1, width = 0
    ) +
    geom_errorbar(aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    colour = "black", width = 0
    ) +
    geom_vline(xintercept = 1.5, linetype = 5, col = "black", size = 0.6) +
    geom_label(aes(x = 1.5, y = 0.18, label = "Munich")) +
    geom_hline(yintercept = 0, color = "red", linetype = 2) +
    xlab("Weeks after Treatment") +
    ylab("Treatment effect") +
    scale_x_discrete(labels = c(
      "weeks_after1" = "Week 1", "weeks_after2" = "Week 2",
      "weeks_after3" = "Week 3",
      "weeks_after4" = "Week 4", "weeks_after5" = "Week 5"
    )) +
    facet_grid(label ~ .) + 
    theme(panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.25)) +
    coord_cartesian(ylim = c(-0.22, 0.26))
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, "_week.pdf"),
         width = 10, height = 3 + 
           length(unique(models$label[models$group==dv_group & models$outcome %in% vars_time])))
  
  print(g1)
}


# All minoritities #####
plots_path <- "output/plots/supp/r_foreign_all/"
models_path <- "output/models/main/"

models <- load_data()

for(dv_group in c("emo", "dist")) {
  
  g1 <- models %>%
    filter(group == dv_group) %>%
    filter(!str_detect(outcome, "pca")) %>%
    ggplot(aes(x = estimate, y = reorder(label, -as.numeric(label)))) +
    plot_options +
    scale_x_continuous(breaks = pretty_breaks(n = 4)) +
    facet_wrap(~outcome_group)
  
  ggsave(g1,
         file = paste0(plots_path, dv_group, ".pdf"),
         width = 10, height = 2.5 + length(unique(models$label[models$group==dv_group])))
}


# Placebo #####

plots_path <- "output/plots/supp/p_placebo/"
models_path <- "output/models/supp/p_placebo/"

placebo_models <- readRDS(paste0(models_path, "models.Rds"))

# load estimated effect
est_models <- readRDS("output/models/main/models.Rds")

placebo_models$estimate <- NA
for(i in unique(placebo_models$outcome)) {
  placebo_models$estimate[placebo_models$outcome == i] <- est_models$estimate[est_models$term=="treat" &
  str_detect(est_models$outcome, i)]
  placebo_models$p_est[placebo_models$outcome == i] <- est_models$p.value[est_models$term=="treat" &
                                                                            str_detect(est_models$outcome, i)]
}

for (dv_group in c("emo", "risk")) {
  g1 <- placebo_models %>%
    filter(group == dv_group) %>%
    left_join(
      gather(as_tibble(labs), "outcome", "label") %>%
        mutate(label = as_factor(label))
    ) %>%
    ggplot(aes(x = estimate_p)) +
      geom_histogram(fill = "lightgrey", color = "black", bins = 15) +
      geom_vline(aes(xintercept = estimate), color = "red", linetype = 1, size = 1) +
      geom_vline(xintercept = 0, color = "black", linetype = 2, size = 0.5) +
      xlab("Placebo Treatment Effects") +
      ylab("Count") +
      facet_wrap(~label) +
      xlim(c(-0.1, 0.1)) + 
    theme(panel.grid.major = element_line(size  = 0.5),
          panel.grid.minor = element_line(size = 0.25))

  ggsave(g1, file = paste0(plots_path, dv_group, "_placebo.pdf"), width = 11, height = 7)
  print(g1)
}



# Specification curve #####
models_path <- "output/models/supp/r_spec_curve/"
plots_path <- "output/plots/supp/r_spec_curve/"

load(paste0(models_path, "full_models.Rda"))

full_models <- full_models %>%
  left_join(
    gather(as_tibble(labs), "outcome", "label") %>%
      mutate(label = as_factor(label))
  )

# top plot
make_coef_plot <- function(data, subset) {

  # plot specification curve
  g1 <- data %>%
    filter(outcome == subset) %>%
    ggplot(aes(x = h_order, y = estimate), fill = "lightgrey", color = "black") +
    geom_ribbon(aes(
      ymin = estimate - 1.645 * std.error,
      ymax = estimate + 1.645 * std.error
    ),
    size = 0.2, alpha = 0.5
    ) +
    geom_line(size = 1) +
    ylab("Treatment Coefficient") +
    geom_hline(yintercept = 0, color = "black", size = 1, linetype = 2) +
    facet_wrap(~label, scales = "free_x") +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 14),
      strip.text = element_text(size = 16),
      axis.title.y = element_text(size = 14)
    )
  return(g1)
}

# bottom plot
# Function to create a specification plot for a single category.
make_spec_plot <- function(data, category, subset) {
  
  data <- data %>%
    filter(outcome == subset)
  
  # category = spec_cols[1] # DEBUG
  specs <- data %>%
    dplyr::select(h_order, category) %>%
    pivot_longer(starts_with(category), names_prefix = paste0(category, "_")) %>%
    mutate(name = factor(name, levels = rev(unique(name))))
  
  if(is.numeric(specs$value)){
    specs <- specs %>% mutate(h_order = ifelse(value==0, NA, h_order))
    spec_plot <- ggplot(specs, aes(x = h_order, y = reorder(name, desc(name)))) +
      geom_point(shape=21, fill = "darkgrey", size = 1, stroke = 0.5, alpha = 0.5) +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), 
            axis.text.y = element_text(size = 13),
            strip.text = element_text(size = 16))
    
    
  } else {
    spec_plot <- ggplot(specs, aes(x = h_order, y = value)) +
      geom_point(shape=21, fill = "darkgrey",size = 1, stroke = 0.5, alpha = 0.5) +
      scale_alpha_continuous(guide = FALSE, range = c(0, 1)) +
      theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), 
            axis.text.y = element_text(size = 13),
            strip.text = element_text(size = 16))
  }
}

# generate plots

for(subset_var in vars_time) {
  
  coef_plot <- make_coef_plot(full_models, subset = subset_var) + 
    coord_cartesian(ylim = c(-0.15, 0.15))
  
  if(subset_var %in% dvs_allbus_z) {
    spec_plots <- lapply(list(
      c("Int. contacts", "East Germany", "Married", "Education", "Sex", "Age")),
      make_spec_plot, data = full_models, subset = subset_var)
  } else {
    spec_plots <- lapply(list(
      c("Int. contacts", "Comm. accomm.", "Refugee", "Syrian", "Iraqi", "Afghan",
        "East Germany", "Sex", "Age")),
      make_spec_plot, data = full_models, subset = subset_var)
    
  }
  combined_plot <- cowplot::plot_grid(plotlist = c(list(coef_plot), spec_plots),
                                      label_size = 14, label_fontface = "italic", vjust = 0.5, hjust = -0.1,
                                      rel_heights = c(10, 9, 2, 3), align = "v", ncol = 1) 
  
  ggsave(combined_plot, file = paste0(plots_path, subset_var, ".pdf"), 
           width = 6, height = 8)
}
