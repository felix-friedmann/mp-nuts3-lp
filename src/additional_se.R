library(dplyr)
library(plm)
library(lmtest)

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

lagVarT <- paste0(target, "_lag1")
lagVarC <- paste0(controlVar, "_lag1")

tmp <- panel %>%
  group_by(NUTSCODE) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(!!lagVarT := dplyr::lag(.data[[target]], n = 1),
         !!lagVarC := dplyr::lag(.data[[controlVar]], n = 1))

results <- list()

for(h in 0:H) {
  
  leadVar <- "y_lead"
  
  tmp2 <- tmp %>%
    group_by(NUTSCODE) %>%
    arrange(YEAR, .by_group = TRUE) %>%
    mutate(!!leadVar := dplyr::lead(.data[[target]], n = h)) %>%
    filter(!is.na(.data[[leadVar]]))
  
  fml <- as.formula(
    paste0(leadVar, " ~ shocks_std:", heteroVar, " + ", lagVarC, " + ", lagVarT)
  )
  
  mod <- plm(fml, data = tmp2, index = c("NUTSCODE", "YEAR"), model = "within", effect = "twoways")

  vcov_dk <- vcovSCC(mod, type = "HC1", maxlag = 1)
  res <- coeftest(mod, vcov. = vcov_dk)
  
  clust_time <- vcovHC(mod, method = "arellano", type = "HC1", cluster = "time")
  ct_time <- coeftest(mod, vcov. = clust_time)
  
  inter_name <- paste0("shocks_std:", heteroVar)
  
  results[[length(results) + 1]] <- data.frame(
    h = h,
    p_dk = round(unname(res[inter_name, "Pr(>|t|)"]), 4),
    p_tc = round(unname(ct_time[inter_name, "Pr(>|t|)"]), 4),
    row.names = NULL
  )
}

results <- do.call(rbind, results)
print(results)