# ---
# DATA CLEANING - ALLBUS DATA
# ---

# Libraries
library(tidyverse)
library(lubridate)
library(foreign)

# load data
allbus_raw <- read.dta("./data/raw/allbus/ALLBUS_ZA5250/ZA5250_v2-1-0.dta", convert.factors = F)
allbus_txt <- readstata13::read.dta13("./data/raw/allbus/ALLBUS_ZA5254/ZA5254_v1-0-0.dta")

# specify terrorist attacks
events <- tibble(
    dates = as_date(c("2016-07-14", "2016-07-18", "2016-07-24")),
    places = c("Nice", "Würzburg", "Ansbach"), pos = c(10, 20, 30)
)

event_date <- events$dates[events$places == "Nice"]

# General cleaning #####

allbus <- allbus_raw %>%
    mutate(
        date = as_date(as.character(xt03)),
        # merging split variable on perceptions of foreigners
        ma01 = ifelse(ma01a == -11, ma01b, ma01a)
    )

# save negative values as NAs
allbus[allbus < 0] <- NA


# Emotions #####

# How angry/pity/sympa/scared are you with XX
# 1: agree fully, 2: agree somewhat, 3: dont agree somewhat, 4; dont agree fully

allbus <- allbus %>%
    mutate(
        pity_asyl = me01,
        angry_asyl = me02,
        sympa_asyl = me03,
        scared_asyl = me04,

        pity_turk = me05,
        angry_turk = me06,
        sympa_turk = me07,
        scared_turk = me08,

        pity_ital = me09,
        angry_ital = me10,
        sympa_ital = me11,
        scared_ital = me12,

        pity_east = me13,
        angry_east = me14,
        sympa_east = me15,
        scared_east = me16,

        pity_jew = me17,
        angry_jew = me18,
        sympa_jew = me19,
        scared_jew = me20,

        pity_pole = me21,
        angry_pole = me22,
        sympa_pole = me23,
        scared_pole = me24
    )

# Risk perceptions #####

# Are Asylum seekers a risk or an opportunity for the future of Germany?
# Recoded so that higher values indicate more risk
# recoded: 1: clear opportunity, 2: somewhat an opportunity, 3: neither/nor, 4: somewhat a risk 5: clear risk

allbus <- allbus %>%
    mutate(
        asyl_risk_state = car::recode(mp16, "5=1; 4=2; 2=4; 1=5"),
        asyl_risk_safety = car::recode(mp17, "5=1; 4=2; 2=4; 1=5"),
        asyl_risk_cohesion = car::recode(mp18, "5=1; 4=2; 2=4; 1=5"),
        asyl_risk_economy = car::recode(mp19, "5=1; 4=2; 2=4; 1=5")
    )


# Social Distance #####

# how different are italians/refugees/turks/jews/poles from germans 1: Not at all 7: very different

allbus <- allbus %>%
    mutate(
        diff_ital = mg01,
        diff_asyl = mg03,
        diff_turk = mg04,
        diff_jew = mg05,
        diff_pole = mg06
    )

# How uncomfortable would you be, having italian/refugee/turk/jew/pole as neighbour?
# recoded so that higher levels mean higher level of discomfort
# 1: very uncomfortable, 7: very uncomfortable

allbus <- allbus %>%
    mutate(
        neighbor_ital = car::recode(mg07, "7=1; 6=2; 5=3; 3=5; 2=6; 1=7"),
        neighbor_asyl = car::recode(mg09, "7=1; 6=2; 5=3; 3=5; 2=6; 1=7"),
        neighbor_turk = car::recode(mg10, "7=1; 6=2; 5=3; 3=5; 2=6; 1=7"),
        neighbor_jew = car::recode(mg11, "7=1; 6=2; 5=3; 3=5; 2=6; 1=7"),
        neighbor_pole = car::recode(mg12, "7=1; 6=2; 5=3; 3=5; 2=6; 1=7")
    )

# Other variables of interest #####

# Limit the immigration of

allbus <- allbus %>%
    mutate(
        immi_east_bin = ifelse(mi01 == 1, 0, 1),
        immi_asyl_bin = ifelse(mi02 == 1, 0, 1),
        immi_eu_bin = ifelse(mi03 == 1, 0, 1),
        immi_turk_bin = ifelse(mi04 == 1, 0, 1),
        immi_asyl_war_bin = ifelse(mi05 == 1, 0, 1),
        immi_asyl_pers_bin = ifelse(mi06 == 1, 0, 1),
        immi_asyl_econ_bin = ifelse(mi07 == 1, 0, 1)
        )


# Contact with refugees
## Coded so that 1: yes, 0: no
## Refugee shelters in close proximity: 1,2: yes (comm. accomm or single ind.), 0: no

allbus <- allbus %>%
    mutate(
        contact_asyl_bin = ifelse(mc11 == 1, 1, 0),
        contact_shelter_bin = ifelse(mc12 %in% c(1, 2), 1, 0),
        contact_for_bin = if_else(mc01 == 1, 1,
            if_else(mc02 == 1, 1,
                if_else(mc03 == 1, 1,
                    if_else(mc04 == 1, 1, 0)
                )
            )
        )
    )



# Demographic variables #####

allbus <- allbus %>%
    mutate(
        female = ifelse(sex == 2, 1, 0),
        agec = ifelse(agec == 1, "18-29",
            ifelse(agec == 2, "30-44",
                ifelse(agec == 3, "45-59",
                    ifelse(agec %in% c(4, 5, 6), "60+", NA)
                )
            )
        ),
        is_german = ifelse(dn01a == 0, 1, 0),
        rel_group = ifelse(rd01 %in% c(1, 2, 3, 4), "christian",
            ifelse(rd01 == 5, "other",
                ifelse(rd01 == 6, "Not religious", NA)
            )
        ),
        leftright = pa01,
        right = ifelse(pa01 > 5, 1, 0),
        interest_in_pol = pa02a,
        far_right_support = ifelse(pv01 %in% c(20, 42), 1, 0),
        abitur = ifelse(educ %in% c(4, 5), 1, 0),
        isced11 = ifelse(isced11 == 1, "primary",
            ifelse(isced11 %in% c(2, 3, 4), "secondary",
                ifelse(isced11 %in% c(5, 6, 7, 8), "tertiary", NA)
            )
        ),
        edu_primary = ifelse(isced11 == "primary", 1, 0),
        edu_secondary = ifelse(isced11 == "secondary", 1, 0),
        edu_tertiary = ifelse(isced11 == "tertiary", 1, 0),
        educ = ifelse(educ %in% c(1, 2), "no degree/haupt",
            ifelse(educ == 3, "Mittlere Reife",
                ifelse(educ %in% c(4, 5, 6), "Abitur", NA)
            )
        ),
        unemp = ifelse(work == 4, 1,
            ifelse(is.na(work), NA, 0)
        ),
        emp = ifelse(work == 1, "employed, full-time",
            ifelse(work %in% c(2, 3), "employed, other",
                ifelse(work == 4, "not working", NA)
            )
        ),
        married = ifelse(mstat %in% c(1, 2), 1,
            ifelse(is.na(mstat), NA, 0)
        ),
        marstat = ifelse(mstat %in% c(1, 6, 7), "married/cohabiting",
            ifelse(mstat %in% c(2, 3, 4, 8, 9), "widowed/separated/divorced",
                ifelse(mstat == 5, "single", NA)
            )
        ),
        inc = ifelse(inc < 0, NA, inc),
        health = ifelse(hs01 < 0, NA, hs01),
        rural = ifelse(gs01 == 4, 1,
            ifelse(is.na(gs01), NA, 0)
        ),
        east = ifelse(eastwest == 2, 1,
            ifelse(eastwest == 1, 0, NA)
        )
    )


# Attempts to contact interviewee
allbus <- allbus %>%
    mutate(intk = as.numeric(sqrt(xs09)))

# Treatment vars #####
allbus <- allbus %>%
    mutate(
        treat = ifelse(date > event_date, 1,
            ifelse(date <= event_date, 0, NA)
        ),
        time = as.numeric(date - (event_date + 1)),
        weeks_to_event = time %/% 7 + 1
    )


# Generating samples of different lengths #####
gen_samples <- function(df, n, event_date) {
    varname <- paste0("sample_", n)
    mutate(df, {{ varname }} := ifelse(date >= (event_date - n) &
        date <= (event_date + n), 1, 0))
}

# vary treatment group size only
gen_samples2 <- function(df, n, event_date) {
    varname <- paste0("sample2_", n)
    mutate(df, {{ varname }} := ifelse(date >= (event_date - 29) &
        date <= (event_date + n), 1, 0))
}


for (i in c(1:35)) {
    allbus <- gen_samples(allbus, i, event_date)
    allbus <- gen_samples2(allbus, i, event_date)

}


# Modify variables #####

# Allbus #####
allbus_bin <- allbus %>%
    select(starts_with(c("asyl_risk"))) %>%
    mutate_all(function(x) ifelse(x >= 4, 1, 0)) %>%
    rename_all(function(x) paste0(x, "_bin"))

allbus <- cbind(allbus, allbus_bin)

allbus_bin <- allbus %>%
    select(starts_with(c("pity", "angry", "sympa", "scared"))) %>%
    mutate_all(function(x) ifelse(x <= 2, 1, 0)) %>%
    rename_all(function(x) paste0(x, "_bin"))

allbus <- bind_cols(allbus, allbus_bin)


# Restricting sample #####

# sample for all non-missing (case-wise deletion)
allbus <- left_join(allbus, allbus %>%
    filter_at(vars(dvs_allbus[!str_detect(dvs_allbus, "pca")]), all_vars(!is.na(.))) %>%
    mutate(sample_no_na = 1) %>%
    select(respid, sample_no_na)) %>% 
    mutate(sample_no_na = ifelse(is.na(sample_no_na), 0, sample_no_na))
    
# case wise deletion by model group
allbus <- allbus %>%
   mutate(sample_emo = ifelse(complete.cases(pity_asyl, angry_asyl, sympa_asyl, scared_asyl), 1, 0),
          sample_risk = ifelse(complete.cases(asyl_risk_cohesion, asyl_risk_safety, 
                                              asyl_risk_economy, asyl_risk_state), 1, 0),
          sample_dist  = ifelse(complete.cases(neighbor_asyl, diff_asyl), 1, 0))

# Filter data to Germans only 
allbus <- allbus %>%
    filter(is_german == 1) 

## save complete data
allbus_all <- allbus

## Save only relevant sample
allbus <- allbus %>%
    filter(complete.cases(treat)) %>%
    filter(date <= event_date + 35)

## generate text sample 
allbus_txt <- allbus_txt %>%
    left_join(allbus %>% 
                  select(respid, date, treat, starts_with("sample_"), all_of(c(vars, vars_allbus)))) %>%
    filter(complete.cases(treat)) %>%
    filter(date <= event_date + 35)

