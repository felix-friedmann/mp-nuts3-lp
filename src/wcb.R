library(dplyr)
library(fixest)
library(fwildclusterboot)
library(rlang)

# =============== Settings =============== #
# Beschäftigung ->                 SNETD_log
# Reallöhne ->                    ROWCDW_log
# Anteil Großunternehmen ->         GU_Value
# Anteil verarbeitendes Gewerbe ->  SS_Value
# Einkommensniveau ->               EN_Value
# Bruttowertschöpfung ->             GVA_log
# Bruttoinlandsprodukt ->            GDP_log
# geldpolitische Schocks ->       shocks_std
# ======================================== #
H <- 4
target <- "SNETD_log" 
heteroVar <- "GU_Value"
controlVar <- "GVA_log"
# ======================================== #

tvar <- sym(target)

tmp <- panel %>%
  arrange(NUTSCODE, YEAR) %>%
  group_by(NUTSCODE) %>%
  mutate(lag_target = lag(!!tvar, 1)) %>%
  ungroup()

for(h in 0:H){
  tmp <- tmp %>%
    arrange(NUTSCODE, YEAR) %>%
    group_by(NUTSCODE) %>%
    mutate(!!paste0("target_h", h) := lead(!!tvar, h)) %>%
    ungroup()
}

tmp <- tmp %>%
  mutate(
    YEAR_FE = as.factor(YEAR),
    YEAR_CL = YEAR
  )

target_cols <- paste0("target_h", 0:4)
keep <- complete.cases(tmp[, c(target_cols, "lag_target", controlVar, heteroVar, "shocks_std")])
tmp_consistent <- tmp[keep, ]

for(h in 0:H) {

  fml <- as.formula(sprintf(
    "target_h%d ~ shocks_std:%s + %s + lag_target",
    h, heteroVar, controlVar
  ))

  mod <- feols(
    fml,
    data  = tmp,
    fixef = c("NUTSCODE", "YEAR_FE")
  )

  wcb <- boottest(
    mod,
    clustid = "YEAR_CL",
    param = paste0("shocks_std:", heteroVar),
    B = 10000
  )

  print(h)
  print(wcb)
  print(coef(mod)[paste0("shocks_std:", heteroVar)]
)
}
