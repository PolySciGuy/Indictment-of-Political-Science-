# 02_cmps_replication.R
# Replication: Foxworth, Ellenwood, and Evans (2024)
# "Surveying Native Americans: Early Lessons from the CMPS"
# PS: Political Science & Politics
# Original code: Stata 18. Rewritten in R.
# Data: CMPS 2020 (accessed via Harvard Dataverse, DOI:10.7910/DVN/DIZ4ZB)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
})

d <- read_dta("CMPS 2020_replication data.dta")

wtd_tab <- function(data, var, wt = "os_weight") {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(value = as_factor(.data[[var]])) %>%
    summarise(n = n(), wtd_n = sum(.data[[wt]], na.rm = TRUE), .groups = "drop") %>%
    mutate(wtd_pct = round(100 * wtd_n / sum(wtd_n), 1))
}

cat("TABLE 1: Native ID Across Question Types\n")
for (v in c("S2_Racer5","Q803","S2_Race_Prime","S2_Nativer2","S2_Nativer3","S2_Nativer4","S2_Nativer5","Q797r5","Q798r5","S11")) {
  cat("\n---", v, "---\n")
  print(wtd_tab(d, v))
}

cat("\nTABLE 2: Characteristics by ID Method\n")
subs <- list(
  "Screener" = d %>% filter(S2_Racer5 == 1),
  "Primary"  = d %>% filter(S2_Race_Prime == 5),
  "Census"   = d %>% filter(Q803 == 3)
)
for (v in c("S2_M","S6","Q14","Q77r1","Q21","Q265","Q57","Q266","Q271","Q819")) {
  cat("\n---", v, "---\n")
  for (nm in names(subs)) {
    cat(" [", nm, "]\n")
    print(wtd_tab(subs[[nm]], v))
  }
}


