library(ARDECO)
library(dplyr)
library(readr)
library(here)
library(tidyr)
library(stringr)

# Zielvariablen (Abschnitt 4.1.1)
# Beschäftigung (SNETD)
df_emp <- ardeco_get_dataset_data(
  'SNETD',
  verbose = TRUE,
  nutscode = 'DE',
  level = '3',
  year = '2000-2019',
  version = 2024
)

df_emp <- df_emp %>%
  select(NUTSCODE, YEAR, VALUE) %>%
  mutate(VALUE = log(VALUE)) %>%
  rename(SNETD_log = VALUE)

# Reallöhne (ROWCDW)
df_wages <- ardeco_get_dataset_data(
  'ROWCDW',
  verbose = TRUE,
  nutscode = 'DE',
  level = '3',
  year = '2000-2019',
  version = 2024,
  unit = 'EUR2020'
)

df_wages <- df_wages %>%
  select(NUTSCODE, YEAR, VALUE) %>%
  mutate(VALUE = log(VALUE)) %>%
  rename(ROWCDW_log = VALUE)

# Heterogenitätsvariablen (Abschnitt 4.1.2)
df_raw <- read_csv2(
  here("data","struktur.csv"),
  col_names = FALSE,
  na = c("", "NA"),
  locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8")
)

df_lau <- read_csv2(
  here("data","laucode.csv"),
  locale = locale(encoding = "UTF-8")
)

kopf_var  <- as.character(unlist(df_raw[1, ]))
kopf_jahr <- as.character(unlist(df_raw[2, ]))

df_long <- df_raw[-c(1, 2), ] %>%
  pivot_longer(
    cols = -c(X1, X2, X3),
    names_to  = "col_index",
    values_to = "Wert_raw"
  ) %>%
  mutate(
    Kennziffer = as.character(X1),
    Raumeinheit = as.character(X2),
    Aggregat = as.character(X3),
    col_index = as.integer(str_remove(col_index, "^X")),
    Variable  = as.character(kopf_var[col_index]),
    Jahr      = suppressWarnings(as.integer(kopf_jahr[col_index])),
    Wert      = parse_number(Wert_raw, locale = locale(decimal_mark = ",", grouping_mark = "."))
  ) %>%
  select(Kennziffer, Variable, Jahr, Wert) %>%
  filter(!is.na(Jahr))

df_lau_clean <- df_lau %>%
  transmute(Kennziffer = substr(LAUCODE, 1, 5), NUTSCODE) %>%
  distinct(Kennziffer, .keep_all = TRUE)

df_long <- df_long %>%
  left_join(df_lau_clean, by = "Kennziffer") %>%
  filter(!is.na(NUTSCODE)) %>%
  select(NUTSCODE, Variable, Jahr, Wert)

jahr_min <- 2000; jahr_max <- 2019
df_long <- df_long %>% filter(Jahr >= jahr_min, Jahr <= jahr_max)

firmsize_avg <- df_long %>%
  filter(Variable == "Großunternehmen") %>%
  group_by(NUTSCODE) %>%
  summarise(Großunternehmen = mean(Wert, na.rm = TRUE) * 10, .groups = "drop")

income_avg <- df_long %>%
  filter(Variable == "Bruttoverdienst") %>%
  group_by(NUTSCODE) %>%
  summarise(Bruttoverdienst = mean(Wert, na.rm = TRUE), .groups = "drop")

sector_avg <- df_long %>%
  filter(Variable %in% c("Anteil Erwerbstätige Verarbeitendes Gewerbe an Industrie",
                         "Erwerbstätige Sekundärer Sektor")) %>%
  pivot_wider(
    id_cols = c(NUTSCODE, Jahr),
    names_from = Variable,
    values_from = Wert
  ) %>%
  mutate(Sector_kombiniert =
           (`Anteil Erwerbstätige Verarbeitendes Gewerbe an Industrie` *
              `Erwerbstätige Sekundärer Sektor`) / 10000) %>%
  group_by(NUTSCODE) %>%
  summarise(Sector_kombiniert = mean(Sector_kombiniert, na.rm = TRUE) * 10, .groups = "drop")

df_hetero <- firmsize_avg %>%
  left_join(income_avg, by = "NUTSCODE") %>%
  left_join(sector_avg, by = "NUTSCODE") %>%
  mutate(Bruttoverdienst = log(Bruttoverdienst)) %>%
  rename(GU_Value = Großunternehmen,
         EN_Value = Bruttoverdienst,
         SS_Value = Sector_kombiniert) %>%
  select(NUTSCODE, GU_Value, SS_Value, EN_Value) %>%
  arrange(NUTSCODE)

# Kontrollvariablen (Abschnitt 4.1.3)
# reale Bruttowertschöpfung (SOVGE/SNPTD)
df_gvaN <- ardeco_get_dataset_data(
  'SOVGE',
  verbose = TRUE,
  nutscode = 'DE',
  level = '3',
  year = '2000-2019',
  version = 2024,
  unit = 'MIO_EUR2020'
)

df_pop <- ardeco_get_dataset_data(
  'SNPTD',
  verbose = TRUE,
  nutscode = 'DE',
  level = '3',
  year = '2000-2019',
  version = 2024
)

df_gvaN <- df_gvaN[, !names(df_gvaN) %in% c("VARIABLE", "VERSIONS", "LEVEL", "UNIT")]
df_pop <- df_pop[, !names(df_pop) %in% c("VARIABLE", "VERSIONS", "LEVEL", "UNIT")]

df_gva <- merge(df_gvaN, df_pop, by = c("YEAR", "NUTSCODE"), suffixes = c("_gva", "_pop"))
df_gva$VALUE <- (df_gva$VALUE_gva / df_gva$VALUE_pop) * 1e6

df_gva <- df_gva %>%
  select(NUTSCODE, YEAR, VALUE) %>%
  mutate(VALUE = log(VALUE)) %>%
  rename(GVA_log = VALUE)

# reales Bruttoinlandsprodukt (SOVGDP)
df_gdp <- ardeco_get_dataset_data(
  'SOVGDP',
  verbose = TRUE,
  nutscode = 'DE',
  level = '3',
  year = '2000-2019',
  version = 2024,
  unit = 'EUR2020'
)

df_gdp <- df_gdp %>%
  select(NUTSCODE, YEAR, VALUE) %>%
  mutate(VALUE = log(VALUE)) %>%
  rename(GDP_log = VALUE)

# Geldpolitische Schocks (Abschnitt 4.2)
shocks <- read_csv(here("data", "shocks_ecb_mpd_me_m.csv"))
shocks <- shocks[, !names(shocks) %in% c("pc1_hf", "STOXX50_hf", "CBI_pm", "CBI_median")]

shocks <- shocks %>%
  filter(year <= 2019, year >= 2000) %>%
  mutate(weight = 13 - month,
         MP_median = MP_median * 100)

shocks_yearly <- shocks %>%
  group_by(year) %>%
  summarise(
    MP_std = sum(MP_median, na.rm = TRUE),
    MP_weight = sum(MP_median * weight),
  )

df_shocks <- shocks_yearly %>%
  mutate(shocks_std = ((MP_std - mean(MP_std, na.rm = TRUE)) / 100),
         shocks_weighted = ((MP_weight - mean(MP_weight, na.rm = TRUE)) / 100)) %>%
  select(year, shocks_std, shocks_weighted) %>%
  rename(YEAR = year)

# Zusammenfassen in ein Panel
codes <- df_emp %>% distinct(NUTSCODE)
years <- df_emp %>% distinct(YEAR)

panel <- expand_grid(NUTSCODE = codes$NUTSCODE,
                     YEAR = years$YEAR)

panel <- panel %>%
  left_join(df_emp, by=c("NUTSCODE","YEAR")) %>%
  left_join(df_wages, by=c("NUTSCODE","YEAR")) %>%
  left_join(df_gva, by=c("NUTSCODE","YEAR")) %>%
  left_join(df_gdp, by=c("NUTSCODE","YEAR")) %>%
  left_join(df_shocks, by="YEAR") %>%
  left_join(df_hetero, by="NUTSCODE")