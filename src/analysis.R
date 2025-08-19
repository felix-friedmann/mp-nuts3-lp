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
# ======================================== #
# alternative Wahl der Schockaggregation
shockVar <- "shocks_std"
# alternative Wahl der Gruppenunterschiede
upper <- 0.9
lower <- 0.1
# zusätzliche quadrierte heteroVar
squared <- FALSE
# zwei Lags für Ziel-/Kontrollvariable
doubleLagT <- FALSE
doubleLagC <- FALSE
# ======================================== #

lagVarT <- paste0(target, "_lag1")
lagVarT2 <- paste0(target, "_lag2")
lagVarC <- paste0(controlVar, "_lag1")
lagVarC2 <- paste0(controlVar, "_lag2")

requiredLags <- c(lagVarT, lagVarC)
if(doubleLagT) requiredLags <- c(requiredLags, lagVarT2)
if(doubleLagC) requiredLags <- c(requiredLags, lagVarC2)

tmp <- panel %>%
  group_by(NUTSCODE) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(!!lagVarT := dplyr::lag(.data[[target]], n = 1),
         !!lagVarT2 := dplyr::lag(.data[[target]], n = 2),
         !!lagVarC := dplyr::lag(.data[[controlVar]], n = 1),
         !!lagVarC2 := dplyr::lag(.data[[controlVar]], n = 2),
         YEAR_FE = as.factor(YEAR),
         YEAR_CL = YEAR) %>%
  drop_na(all_of(requiredLags))

x <- tmp[[heteroVar]]
IQR_1 <- as.numeric(mean(x[x <= quantile(x, lower, na.rm = TRUE)], na.rm = TRUE))
IQR_2 <- as.numeric(mean(x[x >= quantile(x, upper, na.rm = TRUE)], na.rm = TRUE))

lagsT <- if(doubleLagT) paste0(lagVarT, " + ", lagVarT2) else paste0(lagVarT)
lagsC <- if(doubleLagC) paste0(lagVarC, " + ", lagVarC2) else paste0(lagVarC)

dqrng::dqset.seed(10)
set.seed(10)
results <- list()
resultsSq <- list()

for(h in 0:H) {
  
  leadVar <- "y_lead"
  
  tmp2 <- tmp %>%
    group_by(NUTSCODE) %>%
    arrange(YEAR, .by_group = TRUE) %>%
    mutate(!!leadVar := dplyr::lead(.data[[target]], n = h)) %>%
    filter(!is.na(.data[[leadVar]]))
  
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
  
  if(squared) {
    H10 <- quantile(tmp$EN_Value, 0.10, na.rm = TRUE)
    H50 <- quantile(tmp$EN_Value, 0.50, na.rm = TRUE)
    H90 <- quantile(tmp$EN_Value, 0.90, na.rm = TRUE)
    
    resultsSq[[length(resultsSq) + 1]] <- data.frame(
      h = h,
      ep = -beta / (2 * sq),
      m10 = (beta * H10) + (sq * H10^2),
      m50 = (beta * H50) + (sq * H50^2),
      m90 = (beta * H90) + (sq * H90^2),
      dm10 = beta + (2 * sq * H10),
      dm90 = beta + (2 * sq * H90)
    )
  }
}

results_df <- do.call(rbind, results)
rownames(results_df) <- NULL
print(results_df)

if(squared) {
  resultsSq_df <- do.call(rbind, resultsSq)
  rownames(resultsSq_df) <- NULL
  print(resultsSq_df)
}