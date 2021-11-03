# ---
#  Descriptives
# ---

library(tidyverse)
library(tidyquant)
library(kableExtra)
library(cobalt)
library(WeightIt)
library(patchwork)

tables_path <- "output/tables/desc/"
plots_path <- "output/plots/desc/"
models_path <- "output/models/desc/"

theme_set(theme_bw(base_size = 24))


# Events --------------------------------------------------------------------------------------

# specify terrorist attacks
events <- tibble(
    dates = as_date(c("2016-07-14", "2016-07-18", "2016-07-24")),
    places = c("Nice", "Würzburg", "Ansbach"), pos = c(10, 20, 30)
)

event_date <- events$dates[events$places == "Nice"]

# APPLICATIONS FOR ASYLUM  --------------------------------------------------------------------
asyl_apps <- read_csv("./data/raw/asylum registrations/bamf_asylum-registrations.csv")

asyl_apps <- asyl_apps %>%
    gather(year, n_app, `1979`:`2020`) %>%
    rename(type = Jahr) %>%
    na_if("null") %>%
    mutate(
        type = factor(ifelse(type == "Erstanträge", "First applications",
            ifelse(type == "Folgeanträge", "Subsequent applications",
                ifelse(type == "Gesamtanträge", "Total applications", NA)
            )
        ),
        levels = c("Subsequent applications", "First applications", "Total applications")
        ),
        year = as.numeric(year),
        n_app = as.numeric(n_app) / 1000
    ) %>%
    filter(year >= 1995 & year < 2020)

g1 <- ggplot(asyl_apps, aes(x = year, y = n_app, fill = type), color = "black") +
    geom_bar(stat = "identity", position = "stack", color = "black") +
    ylab("Asylum Applications (in 1000s)") +
    scale_fill_manual(values = c("white", "lightgrey")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-10,-10,-10,-10),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.25))

ggsave(paste0(plots_path, "asylum_applications.png"), height = 6, width = 10)

print(g1)

# Interviews per week #####

obs_day_refugee <- refugee_all %>%
    mutate(week = week(date)) %>%
    group_by(week) %>%
    summarise(
        n = n(),
        treat = first(treat),
        sample_29 = first(sample_29)
    ) %>%
    mutate(type = "Refugee Survey")

obs_day_allbus <- allbus_all %>%
    mutate(week = week(date)) %>%
    group_by(week) %>%
    summarise(
        n = n(),
        treat = first(treat),
        sample_29 = first(sample_29)
    ) %>%
    mutate(type = "German Survey")


obs_day <- bind_rows(obs_day_refugee, obs_day_allbus)

date_df <- data.frame(date = seq(min(allbus_all$date),
    max(refugee_all$date),
    by = "day"
)) %>%
    mutate(week = week(date))



obs_day <- full_join(obs_day, date_df, by = "week")


# Treatment period #####
g1 <- ggplot(obs_day, aes(x = date, y = n)) +
    geom_area(color = "black", fill = "white", alpha = 1 / 3) +
    geom_area(
        data = obs_day[obs_day$sample_29 == 1 & obs_day$date <= as.Date("2016-07-14"), ],
        aes(x = date, y = n, fill = "Control"), alpha = 1 / 2
    ) +
    geom_area(
        data = obs_day[obs_day$sample_29 == 1 & obs_day$date >= as.Date("2016-07-14"), ],
        aes(x = date, y = n, fill = "Treated"), alpha = 1 / 2
    ) +
    geom_vline(data = events, aes(xintercept = dates), size = 0.6, linetype = 2, col = "darkgrey") +
    geom_label(
        data = events, aes(label = places, x = dates, y = 150 + (pos * 8)),
        size = 6.5, hjust = "left"
    ) +
    geom_vline(aes(xintercept = event_date), size = 0.6, linetype = 2, col = "red") +
    facet_grid(type ~ .) +
    ylab("Number of interviews per week") +
    scale_fill_grey(start = 0.7, end = 0.1) +
    labs(fill = "Treatment group") +
    theme(
        legend.position = "top",
        axis.title.x = element_blank(),
        panel.grid.major = element_line(size = 0.5),
        panel.grid.minor = element_line(size = 0.25)
    ) +
    scale_x_date()

ggsave(paste0(plots_path, "sample_period.png"), height = 6, width = 10)

print(g1)



# Summary table #####

# Function to create descriptive Tables
summary_table <- function(data, vars,
                          stats = c("N", "Mean", "SD"),
                          na.rm = T, char_vars = F, labels = labs) {
                            
    data <- data %>%
        select(vars)
        
    if(char_vars==T) {
    data <- data %>%
        fastDummies::dummy_cols(remove_selected_columns = T) %>%
        select(contains(vars))
    }    

    data %>%
        rename(!!! labels) %>%
        gather(key = "key", value = "value", factor_key = T) %>%
        group_by(key) %>%
        summarise_at(
            "value",
            list(
                N = ~ length(.),
                Mean = ~ mean(., na.rm = na.rm),
                SD = ~ sd(., na.rm = na.rm),
                Freq. = ~ sum(is.na(.)),
                Prop. = ~ round(sum(is.na(.) / length(.)), 2)
            )
        ) %>%
        select(key, all_of(stats)) %>%
        rename(" " = key) %>%
        mutate_if(is.numeric, round, 2)
}

# Allbus #####
tmp_1 <- allbus %>%
    filter(sample_29 == 1) %>%
    filter(treat == 0) %>%
    select(all_of(c(dvs_allbus, vars_allbus))) %>%
    summary_table(vars = c(dvs_allbus, vars_allbus), labels = labs_allbus)

tmp_2 <- allbus %>%
    filter(sample_29 == 1) %>%
    filter(treat == 1) %>%
    select(all_of(c(dvs_allbus, vars_allbus))) %>%
    summary_table(vars = c(dvs_allbus, vars_allbus), labels = labs_allbus)


cbind(tmp_1, tmp_2[, -1]) %>%
    kbl("html",
        booktabs = T,
        caption = "Summary statistics (German sample) \\label{tab_summary_allbus}"
    ) %>%
    kable_styling(latex_options = c("HOLD_position", position = "center"), font_size = 10) %>%
        pack_rows("Emotions", 1, 5) %>%
        pack_rows("Risk", 6, 10) %>%
        pack_rows("Social distance", 11, 12) %>%
        pack_rows("Independent variables", 13, nrow(tmp_1)) %>%
        add_header_above(c(" " = 1, "Control" = 3, "Treatment" = 3)) %>%
        cat(file = paste0(tables_path, "summary_allbus.html"))

# Soep #####
tmp_1 <- refugee %>%
    filter(sample_29 == 1) %>%
    filter(treat == 0) %>%
    select(all_of(c(dvs_refugee, vars_refugee))) %>%
    summary_table(vars = c(dvs_refugee, vars_refugee), labels = labs_refugee)

tmp_2 <- refugee %>%
    filter(sample_29 == 1) %>%
    filter(treat == 1) %>%
    select(all_of(c(dvs_refugee, vars_refugee))) %>%
    summary_table(vars = c(dvs_refugee, vars_refugee), labels = labs_refugee)


cbind(tmp_1, tmp_2[, -1]) %>%
    kbl("html",
        booktabs = T,
        caption = "Summary statistics (Refugee sample) \\label{tab_summary_refugee}"
    ) %>%
    kable_styling(latex_options = c("HOLD_position", position = "center"), font_size = 10) %>%
    pack_rows("Hostility", 1, 3) %>%
    pack_rows("Well-being", 4, 5) %>%
    pack_rows("Independent variables", 6, 14) %>%
    add_header_above(c(" " = 1, "Control" = 3, "Treatment" = 3)) %>%
        cat(file = paste0(tables_path, "summary_refugee.html"))


# Missingness table #####

# Allbus #####
allbus_all %>%
    filter(sample_29==1) %>%
    summary_table(vars = c(dvs_allbus[!str_detect(dvs_allbus, "pca")], vars_allbus), 
                  labels =labs_allbus[!str_detect(labs_allbus, "pca")], 
    na.rm = F, stats = c("Freq.", "Prop.")) %>%
        kbl("latex",
        booktabs = T,
        caption = "Missing values (German sample) \\label{tab_missing_allbus}"
    ) %>%
    kable_styling(latex_options = c("HOLD_position", position = "center"), font_size = 10) %>%
        pack_rows("Emotions", 1, 4) %>%
        pack_rows("Risk", 5, 8) %>%
        pack_rows("Social distance", 9, 10) %>%
        pack_rows("Independent variables", 11, length(labs_allbus[!str_detect(labs_allbus, "pca")])) %>%
        cat(file = paste0(tables_path, "tab_missing_allbus.tex"))

# Refugee #####
refugee_all %>%
    filter(sample_29==1) %>%
    summary_table(vars = c(dvs_refugee, vars_refugee), labels = labs_refugee, 
    na.rm = F, stats = c("Freq.", "Prop.")) %>%
        kbl("latex",
        booktabs = T,
        caption = "Missing values (Refugee sample) \\label{tab_missing_refugee}"
    ) %>%
    kable_styling(latex_options = c("HOLD_position", position = "center"), font_size = 10) %>%
    pack_rows("Hostility", 1, 3) %>%
    pack_rows("Well-being", 4, 5) %>%
    pack_rows("Independent variables", 6, length(labs_refugee)) %>%
        cat(file = paste0(tables_path, "tab_missing_refugee.tex"))


# Correlation matrix
cor_m <- cor(allbus[allbus$sample_29==1, c(dvs_allbus, vars_allbus)])
rownames(cor_m) <- c("Emotion (factor)", "Angry", "Scared", "Pity", "Affection", 
           "Risk (Factor)", "Welfare", "Cohesion", "Safety", "Economy",
           "Difference", "Neighbour", "Female", "Age", "East", "Int. contacts", 
           "Education", "Married", "Unemployed")
colnames(cor_m) <- c("Emotion (factor)", "Angry", "Scared", "Pity", "Affection", 
            "Risk (Factor)", "Welfare", "Cohesion", "Safety", "Economy",
            "Difference", "Neighbour", "Female", "Age", "East", "Int. contacts", 
            "Education", "Married", "Unemployed")


png(file = paste0(plots_path, "cor_allbus.png"))

corrplot::corrplot(cor_m, 
         type="lower", 
         method="color", 
         addCoef.col="black",
         number.cex = 0.7,
         number.digits = 1,
         tl.col="black",
         col=gray.colors(100))

dev.off()

cor_m <- cor(refugee[refugee$sample_29==1, c(dvs_refugee, vars_refugee)])
rownames(cor_m) <- c("Discrimination", "Anti-immig. worries", "Feeling welcome",
                     "Mental health", "Mental distress", 
                     "Female", "Age", "East", "Int. contacts", 
                     "Refugee", "Syrian", "Iraqi", "Afghan", "Comm. accomm.")
colnames(cor_m) <- c("Discrimination", "Anti-immig. worries", "Feeling welcome",
                     "Mental health", "Mental distress", 
                     "Female", "Age", "East", "Int. contacts", 
                     "Refugee", "Syrian", "Iraqi", "Afghan", "Comm. accomm.")

png(file = paste0(plots_path, "cor_refugee.png"))

corrplot::corrplot(cor_m, 
                   type="lower", 
                   method="color", 
                   addCoef.col="black",
                   number.cex = 0.7,
                   number.digits = 1,
                   tl.col="black",
                   col=gray.colors(100))
dev.off()

# List of worries ####

refugee %>%
    filter(sample_29==1) %>% 
    group_by(treat) %>%
    select(starts_with("worries")) %>% 
    select(ends_with("bin")) %>% 
    gather(key, value, -treat) %>%
    ggplot(aes(x = reorder(key, value), y = value, fill = factor(treat))) +
    geom_bar(stat = "summary", 
             fun = "mean", color="black", position=position_dodge()) +
    scale_x_discrete(labels = c("Anti-immigrant\nsentiment", "Health", "Obtaining\nasylum",
                                "Not returning\nhome", "Having to\nleave Germany", "Economic\nsituation")) +
    ylim(0, 0.8) +
    ylab("% respondents worried about ...") +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.x=element_blank(),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.25),
          legend.position = "top") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
    scale_fill_brewer(palette="Greys", name = "Sample",
                      labels=c("Control", "Treatment"))

ggsave(last_plot(), file = paste0(plots_path, "worries.png"), width = 10, height = 7)

# End
