# ---
# DATA CLEANING
# ---

# Libraries
library(tidyverse)
library(lubridate)
library(foreign)

# load data
refugee_raw <- read.dta("./data/raw/soep/SOEP-CORE_v33.1_stata_bilingual/bgp_refugees.dta", convert.factors = F)
refugee_hhld_raw <- read.dta("./data/raw/soep/SOEP-CORE_v33.1_stata_bilingual/bgh_refugees.dta")
hhld_raw <- read.dta("./data/raw/soep/SOEP-LONG_v33.1_stata_de+en/hgen.dta")
hbrutto_raw <- read.dta("./data/raw/soep/SOEP-LONG_v33.1_stata_de+en/hbrutto.dta", convert.factors = F)
pgen_raw <- read.dta("./data/raw/soep/SOEP-LONG_v33.1_stata_de+en/pgen.dta", convert.factors = F)
health_raw <- read.dta("./data/raw/soep/SOEP-CORE_v33.1_stata_bilingual/health.dta")


# specify terrorist attacks
events <- tibble(
    dates = as_date(c("2016-07-14", "2016-07-18", "2016-07-24")),
    places = c("Nice", "Würzburg", "Ansbach"), pos = c(10, 20, 30)
)

event_date <- events$dates[events$places == "Nice"]


# General #####

# get bundesland data
hhld <- hhld_raw %>%
    filter(syear == 2016) %>%
    select(cid, hid, hgnuts1) %>%
    mutate(hgnuts1 = as.character(hgnuts1))


# get number of ties interviewer contacted household
hbrutto <- hbrutto_raw %>%
    filter(syear == 2016) %>%
    select(cid, hid, intk, bula) %>%
    mutate(
        intk = ifelse(intk < 0, NA, intk),
        intk = sqrt(intk)
    )

# get health data
health <- health_raw %>%
    filter(syear == 2016) %>%
    rename(cid = hhnr, pid = persnr) %>%
    mutate(
        mcs = ifelse(mcs < 0, NA, mcs),
        mh_nbs = as.numeric(mh_nbs), 
        mh_nbs = ifelse(mh_nbs<0, NA, mh_nbs)
    )

# get asylum status
pgen <- pgen_raw %>%
    filter(syear == 2016) %>%
    select(cid, hid, pid, pgisced11, pgstatus_asyl, pgstatus_refu) %>%
    mutate(
        isced11 = ifelse(pgisced11 %in% c(0, 1), "in school, primary",
            ifelse(pgisced11 %in% c(2, 3, 4), "secondary",
                ifelse(pgisced11 %in% c(5, 6, 7, 8), "tertiary", NA)
            )
        ),
        pgstatus_asyl = ifelse(pgstatus_asyl == 1, "refugee/asylum status",
            ifelse(pgstatus_asyl %in% c(2, 3), "tolerated/asked to leave",
                ifelse(pgstatus_asyl == 4, "still waiting", NA)
            )
        ),
        pgstatus_refu = ifelse(pgstatus_refu == 1, "processing application",
            ifelse(pgstatus_refu == 2, "refugee/asylum status",
                ifelse(pgstatus_refu %in% c(3, 4), "tolerated/other", NA)
            )
        )
    )


# Refugee hhld data #####
refugee_hhld <- refugee_hhld_raw %>%
    select(hhnr, hhnrakt, syear, sample1, bghr01, bghr27, bghr37, bghr38, bghr39, hspvers, bghrtagin, bghrmonin) %>%
    transmute(
        cid = hhnr,
        hid = hhnrakt,
        syear = syear,
        sample_survey = as.character(sample1),
        comm_accom = ifelse(bghr01 == "[1] Gemeinschaftsunterkunft", 1, 0),
        accom_type = as_factor(bghr01),
        accom_safety = as_factor(bghr37),
        surround_safety = as_factor(bghr38),
        leistungen_AsylbLG = as_factor(bghr39),
        int_lang = as_factor(hspvers)
    )


refugee_hhld <- left_join(refugee_hhld, hhld) %>%
    left_join(hbrutto) %>%
    rename(
        land = hgnuts1,
        land_nr = bula
    ) %>%
    mutate_if(is.factor, as.character)


# Refugee survey #####

# Dependent variables #####
refugee <- refugee_raw %>%
    mutate(
        cid = hhnr,
        hid = hhnrakt,
        pid = persnr,

        # Discrimination
        # 1: often, 2: sometimes, 3: never
        discrim = bgpr67,

        # Worries over anti-immigrant sentiment
        # 1: many worries, 2: some worries, 3: no worries
        worries_antiimmig = bgpr356,

        # How welcome do you feel: then and now?
        # recode so that higher values mean more welcome
        # recode: 1: not at all, 5: completely
        welcome_arrival = ifelse(bgpr326 == 5, 1,
            ifelse(bgpr326 == 4, 2,
                ifelse(bgpr326 == 3, 3,
                    ifelse(bgpr326 == 2, 4,
                        ifelse(bgpr326 == 1, 5, NA)
                    )
                )
            )
        ),
        welcome_today = ifelse(bgpr327 == 5, 1,
            ifelse(bgpr327 == 4, 2,
                ifelse(bgpr327 == 3, 3,
                    ifelse(bgpr327 == 2, 4,
                        ifelse(bgpr327 == 1, 5, NA)
                    )
                )
            )
        ),

        # Mental distress
        # Made up of 4 indicators: little interest, depressed, nervousness, cant stop worrying
        md_bgpr312 = ifelse(bgpr312 < 0, NA, bgpr312),
        md_bgpr313 = ifelse(bgpr313 < 0, NA, bgpr313),
        md_bgpr314 = ifelse(bgpr314 < 0, NA, bgpr314),
        md_bgpr315 = ifelse(bgpr315 < 0, NA, bgpr315),
        
        # social isolation
        isolate = ifelse(bgpr322 %in% c(1, 2, 3), 1, 0), 
        
        # safety neighbourhood
        safe_neigh = bgpr64, 
        safe_shlt = bgpr65,
    )



# Independent vars #####
refugee <- refugee %>%
    mutate(
        date = as_date(paste0(syear, "-", bgprmonin, "-", bgprtagin)),
        female = ifelse(bgpr_l_0101 == 2, 1,
            ifelse(bgpr_l_0101 == 1, 0, NA)
        ),
        is_married = ifelse(bgpr390 == 2, 1,
            ifelse(bgpr390 < 0, NA, 0)
        ),
        has_kids = ifelse(bgpr_l_401 == 1, 1,
            ifelse(bgpr_l_401 == 2, 0, NA)
        ),
        yob = ifelse(bgpr_l_0103 < 0, NA, bgpr_l_0103),
        year_of_arrival = as_factor(bgpr_l_3401),
        age = ifelse(yob > 0, 2016 - yob, NA),
        agec = ifelse(age >= 18 & age <= 29, "18-29",
            ifelse(age >= 30 & age <= 44, "30-44",
                ifelse(age >= 45 & age <= 59, "45-59",
                    ifelse(age >= 60, "60+", NA)
                )
            )
        ),

        # Citizenship
        citizenship = as.character(as_factor(bgpr0101)),
        citizen_cat = ifelse(citizenship == 19, "Syria",
            ifelse(citizenship == 1, "Afghanistan",
                ifelse(citizenship == 10, "Irak",
                    ifelse(is.na(citizenship), NA, "Other")
                )
            )
        ),
        cit_syria_bin = ifelse(citizen_cat == "Syria", 1,
            ifelse(!is.na(citizen_cat), 0, NA)
        ),
        cit_afghan_bin = ifelse(citizen_cat == "Afghanistan", 1,
            ifelse(!is.na(citizen_cat), 0, NA)
        ),
        cit_iraq_bin = ifelse(citizen_cat == "Irak", 1,
            ifelse(!is.na(citizen_cat), 0, NA)
        ),
        cit_other_bin = ifelse(citizen_cat == "Other", 1,
            ifelse(!is.na(citizen_cat), 0, NA)
        ),

        # Religion
        religion = bgpr350,
        religion_group = ifelse(religion == 4, "Islam",
            ifelse(religion == 7, "Christian",
                ifelse(religion < 0, NA, "Other/None")
            )
        ),
        islam = ifelse(religion == 4, 1,
            ifelse(is.na(religion), NA, 0)
        ),

        # Employment
        unemp = ifelse(bgpr161 == 9, 1,
            ifelse(bgpr161 < 0, NA, 0)
        ),

        # Asylum application
        received_decision = ifelse(bgpr45 == 1, 1,
            ifelse(bgpr45 == 2, 0, NA)
        ),
        decision_type = ifelse(bgpr47 == 1, "refugee",
            ifelse(bgpr47 == 2, "asylum",
                ifelse(bgpr47 == 3, "other prot.",
                    ifelse(bgpr47 %in% c(4, 5), "declined", NA)
                )
            )
        ),
        res_status = bgpr50,
        res_status_cat = ifelse(res_status == 1, "Asylum seeker",
            ifelse(res_status == 3, "Refugee",
                ifelse(res_status == -2, "No decision", "Other")
            )
        ),
        is_refugee = ifelse(res_status == 3, 1,
            ifelse(res_status < 0, NA, 0)
        ),



        # Regular (inter)group contact (with germans and fellow refugees)
        reg_contact_nat = bgpr210,
        weekly_contact_nat_bin = ifelse(bgpr210 %in% c(1, 2), 1,
            ifelse(bgpr210 < 0, NA, 0)
        ),
        reg_contact_ger = bgpr211,
        weekly_contact_ger_bin = ifelse(bgpr211 %in% c(1, 2), 1,
            ifelse(bgpr211 < 0, NA, 0)
        ),

        # Why Germany?
        whyg_family = bgpr_l_3701,
        whyg_friends = bgpr_l_3702,
        whyg_people = bgpr_l_3703,
        whyg_economy = bgpr_l_3704,
        whyg_humanrights = bgpr_l_3705,
        whyg_education = bgpr_l_3706,
        whyg_welfare = bgpr_l_3707,
        whyg_welcome = bgpr_l_3708,
        whyg_asylum = bgpr_l_3709,
        whyg_random = bgpr_l_3710,
        whyg_other = bgpr_l_3711,

        # What do you worry about?
        worries_econ = bgpr354,
        worries_health = bgpr355,
        worries_asyl = bgpr357,
        worries_hastoleave = bgpr358,
        worries_cantreturn = bgpr359,
    )

# only select renamed variables
refugee <- refugee %>%
    dplyr::select(cid:length(.))

# Replace selected variables #####
refugee <- refugee %>%
    # REPLACE NOT VALID (-2) WITH 0 IF VARS BELONG TO SAME QUESTION
    mutate_at(
        vars(starts_with("arrival_"), starts_with("whyg_")),
        function(x) ifelse(x == -2, 0, x)
    )


refugee[refugee %in% c(-1, -3)] <- NA



# Merge refugee surveys #####
refugee <- refugee %>%
    left_join(refugee_hhld) %>%
    left_join(pgen) %>%
    left_join(health)

# Generate east Germany dummy #####
refugee <- refugee %>%
    mutate(east = ifelse(land_nr > 10, 1, 0))

# save factors as characters
refugee <- refugee %>%
    mutate_if(is.factor, as.character)

# Treatment variables #####

refugee <- refugee %>%
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
    refugee <- gen_samples(refugee, i, event_date)
    refugee <- gen_samples2(refugee, i, event_date)
}


# generating dependent variables of interest
refugee <- refugee %>%
    mutate(
        discrim_bin = ifelse(discrim %in% c(1, 2), 1, 0),
        change_in_welcome = welcome_today - welcome_arrival,
        phq_4 = (md_bgpr312 + md_bgpr313 + md_bgpr314 + md_bgpr315) - 4
    )

# generate list of worries (binary)
refugee_bin <- refugee %>%
    select(starts_with("worries_")) %>%
    mutate_all(function(x) ifelse(x %in% c(1, 2), 1, 0)) %>%
    rename_all(function(x) paste0(x, "_bin"))

refugee <- bind_cols(refugee, refugee_bin)

# Restricting sample #####

# sample for all non-missing (case-wise deletion)
refugee <- left_join(refugee, refugee %>%
                        filter_at(vars(dvs_refugee), all_vars(!is.na(.))) %>%
                        mutate(sample_no_na = 1) %>%
                        select(pid, sample_no_na)) %>% 
    mutate(sample_no_na = ifelse(is.na(sample_no_na), 0, sample_no_na))

# case wise deletion by model group
refugee <- refugee %>%
    mutate(sample_host = ifelse(complete.cases(discrim, worries_antiimmig, change_in_welcome), 1, 0),
           sample_mhealth = ifelse(complete.cases(mcs, phq_4), 1, 0))

## restrict sample to dates of analysis
refugee_all <- refugee

refugee <- refugee %>%
    filter(complete.cases(treat)) %>%
    filter(sample_35==1)


rm(
    "refugee_hhld_raw", "hhld_raw", "hbrutto_raw", "pgen_raw", "health_raw", "health", "hhld", "pgen",
    "hbrutto", "refugee_hhld"
)