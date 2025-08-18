library(dplyr)
library(fixest)
library(fwildclusterboot)

# =============== Settings =============== #
# Beschäftigung ->                 SNETD_log
# Reallöhne ->                    ROWCDW_log
# Anteil Großunternehmen ->         GU_Value
# Anteil verarbeitendes Gewerbe ->  SS_Value
# Einkommensniveau ->               EN_Value
# Bruttowertschöpfung ->             GVA_log
# Bruttoinlandsprodukt ->            GDP_log
# geldpolitische Schocks ->       shocks_std
# gewichtete Schocks ->      shocks_weighted
# ======================================== #
H <- 4
target <- "SNETD_log" 
heteroVar <- "GU_Value"
controlVar <- "GVA_log"
shockVar <- "shocks_std"
upper <- 0.9
lower <- 0.1
squared <- FALSE
doubleLagT <- FALSE
doubleLagC <- FALSE
# ======================================== #

lagVarT <- paste0(target, "_lag1")
lagVarT2 <- paste0(target, "_lag2")
lagVarC <- paste0(controlVar, "_lag1")
lagVarC2 <- paste0(controlVar, "_lag2")

tmp <- panel %>%
  group_by(NUTSCODE) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(!!lagVarT := dplyr::lag(.data[[target]], n = 1),
         !!lagVarT2 := dplyr::lag(.data[[target]], n = 2),
         !!lagVarC := dplyr::lag(.data[[controlVar]], n = 1),
         !!lagVarC2 := dplyr::lag(.data[[controlVar]], n = 2),
         YEAR_FE = as.factor(YEAR),
         YEAR_CL = YEAR)

x <- tmp[[heteroVar]]
IQR_1 <- as.numeric(mean(x[x <= quantile(x, lower, na.rm = TRUE)], na.rm = TRUE))
IQR_2 <- as.numeric(mean(x[x >= quantile(x, upper, na.rm = TRUE)], na.rm = TRUE))

lagsT <- if(doubleLagT) paste0(lagVarT, " + ", lagVarT2) else paste0(lagVarT)
lagsC <- if(doubleLagC) paste0(lagVarC, " + ", lagVarC2) else paste0(lagVarC)

dqrng::dqset.seed(10)
results <- list()

for(h in 0:H) {
  
  leadVar <- "y_lead"
  
  tmp2 <- tmp %>%
    group_by(NUTSCODE) %>%
    arrange(YEAR, .by_group = TRUE) %>%
    mutate(!!leadVar := dplyr::lead(.data[[target]], n = h)) %>%
    filter(!is.na(.data[[leadVar]]), !is.na(.data[[lagsT]]), !is.na(.data[[lagsC]]))
  
  fml <- as.formula(
    paste0(leadVar, " ~ ", shockVar, ":", heteroVar, " + ", lagsC, " + ", lagsT,
      " | NUTSCODE + YEAR_FE"
    )
  )
  
  fml2 <- as.formula(
    paste0(leadVar, " ~ ", shockVar, ":", heteroVar, " + ", shockVar, ":I(", heteroVar, "^2) + ", lagsC, " + ", lagsT,
           " | NUTSCODE + YEAR_FE"
    )
  )
  
  reg <- if(squared) fml2 else fml
  mod <- feols(reg, data = tmp2)
  
  wcb <- boottest(
    mod,
    clustid = "YEAR_CL",
    param = paste0(shockVar, ":", heteroVar),
    sign_level = 0.10,
    B = 9999
  )
  
  beta <- coef(mod)[paste0(shockVar, ":", heteroVar)]
  sq <- ifelse(squared, coef(mod)[paste0(shockVar, ":I(", heteroVar, "^2)")], 0)
  diff <- (IQR_2 - IQR_1) * (beta + sq * (IQR_1 + IQR_2))
  mult = ifelse(heteroVar == "EN_Value", 1, 100)
  
  results[[length(results) + 1]] <- data.frame(
    h = h,
    beta = paste0(round(beta * mult, 4), " %"),
    diff = paste0(round(diff * mult, 4), " %"),
    sq = ifelse(squared, round(sq * mult, 4), "NA"),
    R2 = paste0(round(r2(mod, type = "wr2") * 100, 2), " %"),
    ci = paste0("[", round(wcb$conf_int[1] * mult, 4), ", ", round(wcb$conf_int[2] * mult, 4), "]"),
    p = round(wcb$p_val, 4)
  )
}

results_df <- do.call(rbind, results)
rownames(results_df) <- NULL
print(results_df)