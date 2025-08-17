library(dplyr)
library(fixest)

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

lagVar <- paste0(target, "_lag1")

tmp <- panel %>%
  group_by(NUTSCODE) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(!!lagVar := lag(.data[[target]], n = 1))

IQR_H <- as.numeric(
  quantile(tmp[[heteroVar]], 0.9, na.rm=TRUE) -
  quantile(tmp[[heteroVar]], 0.1, na.rm=TRUE)
)

results <- list()

for(h in 0:H) {
  
  leadVar <- "y_lead"
  
  tmp <- tmp %>%
    group_by(NUTSCODE) %>%
    arrange(YEAR, .by_group = TRUE) %>%
    mutate(!!leadVar := lead(.data[[target]], n = h))
  
  fml <- as.formula(
    paste0(leadVar, " ~ shocks_std:", heteroVar, " + ", controlVar, " + ", lagVar,
      " | NUTSCODE + YEAR"
    )
  )
  
  mod <- feols(fml, data = tmp)
  
  coef_name <- paste0("shocks_std:", heteroVar)
  beta <- coef(mod)[coef_name]
  diff <- beta * IQR_H
  mult = ifelse(heteroVar == "EN_Value", 1, 100)
  
  results[[length(results) + 1]] <- data.frame(
    h = h,
    beta = paste0(round(beta * mult, 4), " %"),
    diff = paste0(round(diff * mult, 4), " Pp"),
    sign = ifelse(beta > 0, "oben stärker", "unten stärker")
  )
}

results_df <- do.call(rbind, results)
rownames(results_df) <- NULL
print(results_df)