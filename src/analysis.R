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
upper <- 0.9
lower <- 0.1
squared <- FALSE
# ======================================== #

lagVar <- paste0(target, "_lag1")

tmp <- panel %>%
  group_by(NUTSCODE) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(!!lagVar := lag(.data[[target]], n = 1))

x <- tmp[[heteroVar]]
IQR_1 <- as.numeric(mean(x[x <= quantile(x, lower, na.rm = TRUE)], na.rm = TRUE))
IQR_2 <- as.numeric(mean(x[x >= quantile(x, upper, na.rm = TRUE)], na.rm = TRUE))

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
  
  fml2 <- as.formula(
    paste0(leadVar, " ~ shocks_std:", heteroVar, " + shocks_std:I(", heteroVar, "^2) + ", controlVar, " + ", lagVar,
           " | NUTSCODE + YEAR"
    )
  )
  
  reg <- if(squared) fml2 else fml
  mod <- feols(reg, data = tmp)
  
  beta <- coef(mod)[paste0("shocks_std:", heteroVar)]
  sq <- ifelse(squared, coef(mod)[paste0("shocks_std:I(", heteroVar, "^2)")], 0)
  diff <- (IQR_2 - IQR_1) * (beta + sq * (IQR_1 + IQR_2))
  
  mult = ifelse(heteroVar == "EN_Value", 1, 100)
  
  results[[length(results) + 1]] <- data.frame(
    h = h,
    beta = paste0(round(beta * mult, 4), " %"),
    diff = paste0(round(diff * mult, 4), " %"),
    sign = ifelse(diff > 0, "oben stärker", "unten stärker"),
    sq = ifelse(squared, round(sq * mult, 4), "NA"),
    R2 = paste0(round(r2(mod, type = "wr2") * 100, 2), " %")
  )
}

results_df <- do.call(rbind, results)
rownames(results_df) <- NULL
print(results_df)