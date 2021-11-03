
# Gen project structure ### 
for(f in c("data", "output")) {
  dir.create(f)
  if (f == "data") {
    for(g in c("raw", "edit")) {
      dir.create(file.path(f, g))
      if(g == "raw") {
        for(h in c("soep", "allbus", "asylum registrations"))
          dir.create(file.path(f, g, h))
      }
    }
  } else if (f == "output") {
    for(i in c("models", "plots", "tables")) {
      dir.create(file.path(f, i))
      for(j in c("desc", "main", "supp"))
        dir.create(file.path(f, i, j)) 
      if(j == "supp") {
        for(k in c("p_placebo", "r_brexit", "r_dvs_other", "r_east_west",
                   "r_logit", "r_matching", "r_na", "r_na_group", 
                   "r_no_controls", "r_reachability", "r_spec_curve", 
                   "r_time_trend", "r_treat_alt_periods", "r_treat_by_week", 
                   "r_treat_no_nice", "r_treat_ordinal")) {
          dir.create(file.path(f, i, j, k)) 
        }
      }
    }
  }
}
