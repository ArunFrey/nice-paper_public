# ---
# Variable Names
# ---

# Dependent variables #####

# Allbus
dvs_allbus <- c(
    "emo_asyl_pca", "angry_asyl_bin", "scared_asyl_bin", "pity_asyl_bin", "sympa_asyl_bin",
    "asyl_risk_pca", "asyl_risk_state_bin", "asyl_risk_cohesion_bin", "asyl_risk_safety_bin",
    "asyl_risk_economy_bin",
    "diff_asyl", "neighbor_asyl"
)

dvs_allbus_cont <- c(
    "angry_asyl", "scared_asyl", "pity_asyl", "sympa_asyl",
    "asyl_risk_state", "asyl_risk_cohesion", "asyl_risk_safety", "asyl_risk_economy"
)

dvs_allbus_z <- c(
    "z_emo_asyl_pca", "angry_asyl_bin", "scared_asyl_bin", "pity_asyl_bin", "sympa_asyl_bin",
    "z_asyl_risk_pca", "asyl_risk_state_bin", "asyl_risk_cohesion_bin", "asyl_risk_safety_bin",
    "asyl_risk_economy_bin", 
    "z_diff_asyl", "z_neighbor_asyl"
)


# REFUGEE VARIABLES
dvs_refugee <- c(
    "discrim_bin", "worries_antiimmig_bin", "change_in_welcome",
    "mcs", "phq_4"
)

dvs_refugee_z <- c(
    "discrim_bin", "worries_antiimmig_bin", "z_change_in_welcome",
    "z_mcs", "z_phq_4"
)


# Emotions #####
dvs_emo <- c(
    "z_emo_asyl_pca",
    "scared_asyl_bin", "angry_asyl_bin", "pity_asyl_bin", "sympa_asyl_bin",
    "scared_ital_bin", "angry_ital_bin", "pity_ital_bin", "sympa_ital_bin",
    "scared_turk_bin", "angry_turk_bin", "pity_turk_bin", "sympa_turk_bin",
    "scared_pole_bin", "angry_pole_bin", "pity_pole_bin", "sympa_pole_bin",
    "scared_jew_bin", "angry_jew_bin", "pity_jew_bin", "sympa_jew_bin"
)

# Risks #####
dvs_risk <- c(
    "z_asyl_risk_pca",
    "asyl_risk_safety_bin", "asyl_risk_cohesion_bin",
    "asyl_risk_state_bin", "asyl_risk_economy_bin"
)

# Social Distance #####
dvs_dist <- c(
    "z_neighbor_asyl", "z_neighbor_ital", "z_neighbor_pole",
    "z_neighbor_turk", "z_neighbor_jew",
    "z_diff_asyl", "z_diff_ital", "z_diff_pole", "z_diff_turk", "z_diff_jew"
)

# Hostility #####
dvs_host <-  c("discrim_bin", "worries_antiimmig_bin", "z_change_in_welcome")

# Mental Health #####
dvs_mhealth <-  c( "z_phq_4", "z_mcs")

# Independent variables #####

vars <- c("female", "age", "east", "intk")

vars_z <- c("female", "z_age", "east", "z_intk")

vars_allbus <- append(vars, c("edu_tertiary", "married", "unemp"))

vars_refugee <- append(vars, c(
    "is_refugee", "cit_syria_bin", "cit_iraq_bin",
    "cit_afghan_bin", "comm_accom"
))

vars_allbus_z <- append(vars_z, c("edu_tertiary", "married", "unemp"))

vars_refugee_z <- append(vars_z, c(
    "is_refugee", "cit_syria_bin", "cit_iraq_bin",
    "cit_afghan_bin", "comm_accom"
))


# Labels #####
labs <- list(
    "treat" = "Treatment",
    "treat_ordinalNice" = "Treatment (Nice)",
    "treat_ordinalWürzburg" = "Treatment (Würzburg)",
    "treat_ordinalAnsbach" = "Treatment (Ansbach)",
    "weeks_after1" = "Week 1",
    "weeks_after2" = "Week 2",
    "weeks_after3" = "Week 3",
    "weeks_after4" = "Week 4",
    "emo_asyl_pca" = "Emotions\n(factor)",
    "z_emo_asyl_pca" = "Emotions\n(factor)",
    "scared_asyl_bin" = "Fear",
    "angry_asyl_bin" = "Anger",
    "pity_asyl_bin" = "Pity",
    "sympa_asyl_bin" = "Affection",
    "scared_ital_bin" = "Fear",
    "angry_ital_bin" = "Anger",
    "pity_ital_bin" = "Pity",
    "sympa_ital_bin" = "Affection",
    "scared_jew_bin" = "Fear",
    "angry_jew_bin" = "Anger",
    "pity_jew_bin" = "Pity",
    "sympa_jew_bin" = "Affection",
    "scared_pole_bin" = "Fear",
    "angry_pole_bin" = "Anger",
    "pity_pole_bin" = "Pity",
    "sympa_pole_bin" = "Affection",
    "scared_turk_bin" = "Fear",
    "angry_turk_bin" = "Anger",
    "pity_turk_bin" = "Pity",
    "sympa_turk_bin" = "Affection",
    "asyl_risk_pca" = "Risks\n(factor)",
    "z_asyl_risk_pca" = "Risks\n(factor)",
    "asyl_risk_safety_bin" = "Safety",
    "asyl_risk_cohesion_bin" = "Cohesion",
    "asyl_risk_economy_bin" = "Economy",
    "asyl_risk_state_bin" = "Welfare",
    "diff_asyl" = " Difference",
    "z_diff_asyl" = "Difference",
    "neighbor_asyl"= "Neighbour",
    "z_neighbor_asyl" = "Neighbour",
    "diff_ital" = " Difference",
    "z_diff_ital" = "Difference",
    "neighbor_ital" = "Neighbour",
    "z_neighbor_ital" = "Neighbour",
    "diff_jew" = "Difference",
    "z_diff_jew" = "Difference",
    "neighbor_jew" = "Neighbour",
    "z_neighbor_jew" = "Neighbour",
    "diff_pole" = "Difference",
    "z_diff_pole" = "Difference",
    "neighbor_pole" = "Neighbour",
    "z_neighbor_pole" = "Neighbour",
    "diff_turk" = "Difference",
    "z_diff_turk" = "Difference",
    "neighbor_turk" = "Neighbour",
    "z_neighbor_turk" = "Neighbour",
    "z_health" = "Subj. health",
    "pride_nation" = "National pride", 
    "close_country" = "Solidarity (Germany)",     
    "close_eu" = "Solidarity (EU)", 
    "immi_asyl_bin" = "Restrict\nimmigration",
    "immi_asyl_war_bin" = "War",
    "immi_asyl_pers_bin" = "Political\npersecution",
    "immi_asyl_econ_bin" = "Economic\nsituation",
    "discrim_bin" = "Experienced\ndiscrimination",
    "worries_antiimmig_bin" = "Anti-immigrant\nworries",
    "change_in_welcome" = "Feeling\nwelcome",
    "z_change_in_welcome" = "Feeling\nwelcome",
    "mcs" = "Mental health",
    "z_mcs" = "Mental health",
    "phq_4" = "Mental distress",
    "z_phq_4" = "Mental distress",
    "health" = "Subjective\nhealth",
    "is_refugee" = "Refugee",
    "cit_syria_bin" = "Syrian",
    "cit_iraq_bin" = "Iraqi",
    "cit_afghan_bin" = "Afghan",
    "comm_accom" = "Comm. accom.",
    "female" = "Female",
    "age" = "Age",
    "z_age" = "Age",
    "east" = "East",
    "intk" = "Int. contacts",
    "z_intk" = "Int. contacts",
    "edu_tertiary" = "Tertiary",
    "married" = "Married",
    "unemp" = "Not working",
    "time" = "Time",
    "treat:time" = "Treat x time"
)


labs_allbus <- list(
    "Neg. emotions (factor)" = "emo_asyl_pca",
    "Anger" = "angry_asyl_bin",
    "Fear" = "scared_asyl_bin",
    "Pity" = "pity_asyl_bin",
    "Affection" = "sympa_asyl_bin",
    "Risk (factor)" = "asyl_risk_pca",
    "Welfare state" = "asyl_risk_state_bin",
    "Social cohesion" = "asyl_risk_cohesion_bin",
    "Safety" = "asyl_risk_safety_bin",
    "Economy" = "asyl_risk_economy_bin",
    "Cultural difference" = "diff_asyl",
    "Neighbour" = "neighbor_asyl",
    "Female" = "female",
    "Age" = "age",
    "East" = "east",
    "Int. contacts" = "intk",
    "Tertiary" = "edu_tertiary",
    "Married" = "married",
    "Not working" = "unemp"
)

labs_refugee <- list(
    "Discrimination" = "discrim_bin",
    "Anti-immig. worries" = "worries_antiimmig_bin",
    "Feeling welcome" = "change_in_welcome",
    "Mental health" = "mcs", 
    "Mental distress" = "phq_4", 
    "Female" = "female",
    "Age" = "age",
    "East" = "east",
    "Int. contacts" = "intk",
    "Refugee" = "is_refugee",
    "Syrian" = "cit_syria_bin",
    "Iraqi" = "cit_iraq_bin",
    "Afghan" = "cit_afghan_bin", 
    "Comm. accom." = "comm_accom"
)