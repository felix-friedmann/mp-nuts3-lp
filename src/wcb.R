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

lagVar <- paste0(target, "_lag1")

tmp2 <- panel %>%
  group_by(NUTSCODE) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    !!lagVar := dplyr::lag(.data[[target]], n = 1),
    YEAR_FE = as.factor(YEAR),
    YEAR_CL = YEAR
  )

dqrng::dqset.seed(10)
results <- list()

for(h in 0:H) {
  
  leadVar <- "y_lead"
  
  tmp2 <- tmp2 %>%
    group_by(NUTSCODE) %>%
    arrange(YEAR, .by_group = TRUE) %>%
    mutate(!!leadVar := dplyr::lead(.data[[target]], n = h))
  
  
  fml <- as.formula(
    paste0(leadVar, " ~ shocks_std:", heteroVar, " + ", controlVar, " + ", lagVar,
           " | NUTSCODE + YEAR_FE"
    )
  )
  
  mod <- feols(fml, data = tmp2)

  wcb <- boottest(
    mod,
    clustid = "YEAR_CL",
    param = paste0("shocks_std:", heteroVar),
    sign_level = 0.10,
    B = 9999
  )
  
  mult = ifelse(heteroVar == "EN_Value", 1, 100)

  results[[length(results) + 1]] <- data.frame(
    h = h,
    beta = paste0(round(wcb$point_estimate * mult, 4), "%"),
    p = round(wcb$p_val, 4),
    ci = paste0("[", round(wcb$conf_int[1] * mult, 4), ", ", round(wcb$conf_int[2] * mult, 4), "]")
  )
}


avg_df <- results_df %>%
  summarise(avg_b = mean(beta, na.rm = TRUE), .groups = "drop",
            avg_d = mean(diff, na.rm = TRUE), .groups = "drop",
            max_d = max(diff, na.rm = TRUE), .groups = "drop") %>%
  select(avg_b, avg_d, max_d)


results_df <- do.call(rbind, results)
rownames(results_df) <- NULL
print(results_df)