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
  quantile(tmp$EN_Value, 0.9, na.rm=TRUE) -
  quantile(tmp$EN_Value, 0.1, na.rm=TRUE)
)

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
  
  print(paste0(
    "h = ", h,
    " | gamma = ", beta,
    " | diff = ", diff,
    " | ", ifelse(beta > 0, "oben stärker", "unten stärker")
  ))
}