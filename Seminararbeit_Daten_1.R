

# Pakete laden (installiere ggf. vorher mit install.packages)
library(readr)
library(readxl)
library(nnet)         # für multinom()
library(dplyr)
library(caret)        # für train/test split und confusion matrix
library(ggplot2)
library(forcats)
library(tidyr)
library(tibble)
library(stringr)
library(broom)
library(kableExtra)
library(xtable)
library(strucchange)
library(modelsummary)
library(visreg)
library(lmtest)
library(sandwich)




arbeitslosenquote <- read_csv2(
  "Arbeitslosenquote.csv",
  skip   = 7,
  locale = locale(encoding = "UTF-8", decimal_mark = ",")
)

raw <- arbeitslosenquote[, -c(4:20)] %>% t.data.frame()
years   <- as.character(raw[2, 1:30])
regions <- as.character(raw[1, c(1, 32, 62)])
regions<- as.character(c("Unemp_Deutschland", regions[2:3]))
a <- as.numeric(raw[3,  1:30])
b <- as.numeric(raw[3, 32:61])
c <- as.numeric(raw[3, 62:91])
num_mat <- rbind(a, b, c)
rownames(num_mat) <- regions
colnames(num_mat) <- years
arbeitslosenquote <- as.data.frame(num_mat)


region_names <- rownames(arbeitslosenquote)

uq_wide <- arbeitslosenquote %>%
  rownames_to_column("Region") %>%
  pivot_longer(-Region, names_to="Jahr", values_to="Unemp_rate") %>%
  mutate(
    Jahr       = as.integer(Jahr),
    Unemp_rate = as.numeric(Unemp_rate)
  ) %>%
  pivot_wider(names_from=Region, values_from=Unemp_rate) %>%
  rename_with(~ "Unemp_old", .cols = all_of(region_names[2])) %>%
  rename_with(~ "Unemp_new", .cols = all_of(region_names[3]))
arbeitslosenquote<- as.data.frame(uq_wide)
rm(num_mat)
rm(raw)
rm(uq_wide)


## R-Code zum Einlesen und Aufbereiten der Datei BIP.csv
file_bip <- "~/Desktop/Uni/Seminar/Daten/BIP.csv"

# --- 1) Spaltennamen einlesen ---
# Zeile 7: Bundesländer
header_states <- readLines(file_bip, n = 7)[7]
elems_states <- strsplit(header_states, ";")[[1]]
state_names <- elems_states[seq(2, length(elems_states), by = 2)]
state_names <- state_names[state_names != ""]

# Zeile 6: Gesamtdeutschland-Label
header_group <- readLines(file_bip, n = 6)[6]
elems_group <- strsplit(header_group, ";")[[1]]
group_labels <- elems_group[elems_group != ""]
country <- tail(group_labels, 1)

# Spaltennamen definieren
col_names <- c("Jahr", state_names, country)

# --- 2) Rohdaten einlesen ---
# skip = 7 => ab der ersten Datenzeile (1991)
raw <- read.csv2(
  file = file_bip,
  skip = 7,
  header = FALSE,
  stringsAsFactors = FALSE
)

# --- 3) Nur numerische Spalten auswählen ---
# Jahr (Spalte 1) und jede zweite Spalte ab 2 (Werte, nicht Einheiten)
value_cols <- c(1, seq(2, ncol(raw), by = 2))
data_num <- raw[, value_cols]

# --- 4) Spalten umbenennen ---
names(data_num) <- col_names

# --- 5) Typkonvertierung ---
data_num$Jahr <- as.integer(data_num$Jahr)
for (i in 2:ncol(data_num)) {
  data_num[[i]] <- as.numeric(data_num[[i]])
}

# --- 6) Optional: Spalten mit ausschließlich NA entfernen ---
bip_clean <- data_num[, sapply(data_num, function(x) !all(is.na(x)))]
bip_clean <- bip_clean[rowSums(!is.na(bip_clean[,-1])) > 0, ]
head(bip_clean)

# --- 6) Aggregation in “alte” vs. “neue” Bundesländer ---
old_states <- c(
  "Baden-Württemberg","Bayern","Bremen","Hamburg","Hessen",
  "Niedersachsen","Nordrhein-Westfalen","Rheinland-Pfalz",
  "Saarland","Schleswig-Holstein"
)
new_states <- c(
  "Brandenburg","Mecklenburg-Vorpommern",
  "Sachsen","Sachsen-Anhalt","Thüringen"
)

bip_group <- bip_clean %>%
  mutate(
    GDP_old = rowSums(select(., all_of(old_states)), na.rm = TRUE),
    GDP_new = rowSums(select(., all_of(new_states)), na.rm = TRUE),
    GDP_Insgesamt_berechnet= GDP_old+GDP_new, na.rm=TRUE,
    GDP_Insgesamt_offiziell= bip_clean$Deutschland, na.rm=TRUE
  ) %>%
  select(Jahr, GDP_old, GDP_new, GDP_Insgesamt_berechnet ,GDP_Insgesamt_offiziell)



# --- 7) Ergebnis prüfen ---
head(bip_group)
rm(data_num, raw)


file_he <- "~/Desktop/Uni/Seminar/Daten/Haushaltseinkommen.csv"

# 1) Einlesen der Jahre aus Zeile 6
years_line <- readLines(file_he, n = 6)[6]
year_elems <- strsplit(years_line, ";")[[1]]
raw_years <- year_elems[seq(5, length(year_elems), by = 2)]
years     <- raw_years[grepl("^\\d{4}$", raw_years)]

# 2) Rohdaten laden (skip = 6 Meta‑Zeilen)
raw_he <- read.csv2(
  file            = file_he,
  skip            = 6,
  header          = FALSE,
  stringsAsFactors= FALSE
)

# 3) Meta‑ und Werte‑Spalten extrahieren
val_pos <- seq(5, by = 2, length.out = length(years))     # ← richtiger Name
he_values <- raw_he[, c(1:4, val_pos)]
names(he_values)[1:4] <- c("Region", "Kategorie", "Unterkategorie", "Einheit")
names(he_values)[5:ncol(he_values)] <- years

# 4) Hierarchien auffüllen
he_values <- he_values %>%
  mutate(across(c(Region, Kategorie, Unterkategorie), ~na_if(.x, ""))) %>%
  fill(Region, Kategorie, Unterkategorie, .direction = "down")

# 5) Maßnahme bestimmen
he_values <- he_values %>%
  mutate(Measure = if_else(!is.na(Unterkategorie), Unterkategorie, Kategorie))

# 6) Werte‑Spalten numerisch konvertieren
for (i in seq_along(years)) {
  he_values[[ years[i] ]] <- as.numeric(he_values[[ years[i] ]])
}

# 7) In long‑Format bringen
he_long <- he_values %>%
  pivot_longer(
    cols      = all_of(years),
    names_to  = "Jahr",
    values_to = "Wert"
  ) %>%
  mutate(Jahr = as.integer(Jahr))

# 8) Breite Tabellen pro Maßnahme
income_tables <- he_long %>%
  group_by(Measure) %>%
  group_map(~ pivot_wider(.x, names_from = Region, values_from = Wert), .keep = TRUE)
names(income_tables) <- unique(he_long$Measure)

Haushalts_nettoeinkommen<- income_tables[["Haushaltsnettoeinkommen"]]

Haushalts_nettoeinkommen<- Haushalts_nettoeinkommen %>%
  select(Jahr, `Früheres Bundesgebiet`, `Neue Länder`, Insgesamt)
colnames(Haushalts_nettoeinkommen)<- c("Jahr", "Haushaltsnetto_old", "Haushaltsnetto_new", "Haushaltsnetto_tot")
# Alle Konsumausgaben-Spalten (außer Jahr) multiplizieren
Haushalts_nettoeinkommen[, 2:4] <- Haushalts_nettoeinkommen[, 2:4] * 12

# Schritt 3: (Optional) Zeige die ersten paar Zeilen an
head(Haushalts_nettoeinkommen)

# Schritt 1: Extrahiere den Eintrag "Andere Ausgaben"
Private_Konsumausgaben <- income_tables[["Andere Ausgaben"]]

# Schritt 2: Wähle nur die gewünschten Spalten aus
Private_Konsumausgaben <- Private_Konsumausgaben %>%
  select(Jahr, `Früheres Bundesgebiet`, `Neue Länder`, Insgesamt)

colnames(Private_Konsumausgaben)<- c("Jahr", "Konsumausgaben_old", "Konsumausgaben_new", "Konsumausgaben_tot")
# Alle Konsumausgaben-Spalten (außer Jahr) multiplizieren
Private_Konsumausgaben[, 2:4] <- Private_Konsumausgaben[, 2:4] * 12

# Schritt 3: (Optional) Zeige die ersten paar Zeilen an
head(Private_Konsumausgaben)


df_Bruttoeinkommen <- read_excel("~/Desktop/Uni/Seminar/Daten/Durchschnittliche Jahreseinkommen.xlsx", sheet = "6.1")
colnames(df_Bruttoeinkommen)<- df_Bruttoeinkommen[2,]
df_Bruttoeinkommen<- df_Bruttoeinkommen[-c(1:4),]
df_Bruttoeinkommen<- df_Bruttoeinkommen[c(1:34),]
# 1) Zuerst: automatisch passende Spalten in numeric konvertieren
df_conv <- data.frame(
  lapply(df_Bruttoeinkommen, function(x) {
    # Falls Character oder Factor: versuche as.numeric
    if (is.character(x) || is.factor(x)) {
      num <- suppressWarnings(as.numeric(as.character(x)))
      # Wenn mindestens ein Wert erfolgreich konvertiert wurde,
      # nehmen wir die ganze Spalte als numeric
      if (sum(!is.na(num)) > 0) return(num)
    }
    # Ansonsten Spalte unverändert zurückgeben
    x
  }),
  stringsAsFactors = FALSE
)

# 2) Jetzt alle numeric-Spalten auf 2 Nachkommastellen runden
df_Bruttoeinkommen <- data.frame(
  lapply(df_conv, function(x) {
    if (is.numeric(x)) round(x, 2) else x
  }),
  stringsAsFactors = FALSE
)



df_Bruttoeinkommen <- df_Bruttoeinkommen %>%
  mutate(
    Bruttoeinkommen_old = df_Bruttoeinkommen$Westdeutschland..ohne.Berlin,
    Bruttoeinkommen_new = df_Bruttoeinkommen$Ostdeutschland..ohne.Berlin,
    Bruttoeinkommen_Insgesammt= df_Bruttoeinkommen$Deutschland,
  ) %>%
  select(Jahr, Bruttoeinkommen_old, Bruttoeinkommen_new, Bruttoeinkommen_Insgesammt)


# … dein Einlesen von df_Bruttolohn_pro_Stunde bleibt gleich …
df_Bruttolohn_pro_Stunde <- read_excel(
  "~/Desktop/Uni/Seminar/Daten/Durchschnittliche Jahreseinkommen.xlsx",
  sheet = "8.1")

# (deine Namens‑ und Zeilenbereinigung)
colnames(df_Bruttolohn_pro_Stunde) <- df_Bruttolohn_pro_Stunde[2,]
df_Bruttolohn_pro_Stunde <- df_Bruttolohn_pro_Stunde[-c(1:4),]
df_Bruttolohn_pro_Stunde <- df_Bruttolohn_pro_Stunde[c(1:24),]

# … dein Einlesen und erste Bereinigungen …

# 0) Alle Spalten mit doppelten Namen entfernen (nur die erste behalten)
df_Bruttolohn_pro_Stunde <- df_Bruttolohn_pro_Stunde[
  , !duplicated(colnames(df_Bruttolohn_pro_Stunde))
]

# 1) Spaltennamen säubern (Newlines → Space, überschüssige Leerzeichen weg)
names(df_Bruttolohn_pro_Stunde) <- names(df_Bruttolohn_pro_Stunde) %>%
  stringr::str_replace_all("[\r\n]", " ") %>%
  stringr::str_squish()

# 2) Jetzt die beiden Gruppen‑Spalten korrekt in numeric umwandeln
df_Bruttolohn_pro_Stunde <- df_Bruttolohn_pro_Stunde %>%
  mutate(
    Jahr                           = as.integer(Jahr),
    `Westdeutschland ohne Berlin` = as.numeric(`Westdeutschland ohne Berlin`),
    `Ostdeutschland ohne Berlin`  = as.numeric(`Ostdeutschland ohne Berlin`),
    Deutschland = as.numeric(Deutschland)
  )

wage_group <- df_Bruttolohn_pro_Stunde %>%
  transmute(
    Jahr     = Jahr,
    Wage_old = round(`Westdeutschland ohne Berlin`, 2),
    Wage_new = round(`Ostdeutschland ohne Berlin`, 2),
    Wage_Insgesammt = round(Deutschland, 2)
  )

# Kontrolle
print(head(wage_group))







# 1) Einlesen ab der Zeile mit Land + Jahres‑Header (skip = 3)
df_el <- read_excel(
  "~/Desktop/Uni/Seminar/Daten/AK NE Nachhaltigkeitsindikatoren.xlsx",
  sheet     = "4_1",
  skip      = 3,
  col_names = TRUE
)

# 2) Erste Spalte auf "Land" umbenennen
names(df_el)[1] <- "Land"

# 3) Spaltennamen säubern (Fußnoten‑Entfernung)
names(df_el) <- names(df_el) %>%
  str_replace_all("\\s*\\(.*?\\)", "") %>%
  str_trim()

# 4) Jahres‑Spalten manuell festlegen (2005–2023)
years     <- as.character(2005:2023)
year_cols <- intersect(names(df_el), years)

# 5) **Vor** dem Pivot: wirklich ALLE diese Spalten in Character umwandeln
df_el <- df_el %>%
  mutate(across(all_of(year_cols), as.character))

# 6) Pivot in long‑Format, mit values_transform, um sicher als Character zu bekommen
df_el_long <- df_el %>%
  select(Land, all_of(year_cols)) %>%
  pivot_longer(
    cols             = all_of(year_cols),
    names_to         = "Jahr",
    values_to        = "EarlyLeavers",
    values_transform = list(EarlyLeavers = as.character)
  )

# 7) Nun sauber parsen und Region zuordnen
df_el_region <- df_el_long %>%
  mutate(
    # Komma → Punkt, alles außer Ziffern+Punkt entfernen
    EL_clean     = str_replace_all(EarlyLeavers, ",", ".") %>%
      str_remove_all("[^0-9\\.]"),
    EarlyLeavers = as.numeric(EL_clean),
    Jahr         = as.integer(Jahr),
    Region       = case_when(
      Land %in% old_states ~ "old",
      Land %in% new_states ~ "new",
      TRUE                ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region) & !is.na(EarlyLeavers)) %>%
  group_by(Region, Jahr) %>%
  summarise(
    EarlyLeavers = mean(EarlyLeavers, na.rm = TRUE),
    .groups      = "drop"
  )

# 8) Kontrolle
head(df_el_region)









# 1) Einlesen ab der Zeile mit „Land,2005,…,2023“
df_ter <- read_excel(
  "~/Desktop/Uni/Seminar/Daten/AK NE Nachhaltigkeitsindikatoren.xlsx",
  sheet     = "4_2",
  skip      = 3,
  col_names = TRUE
)

# 2) Erste Spalte auf „Land“ umbenennen
names(df_ter)[1] <- "Land"

# 3) Fußnoten in Spaltennamen entfernen
names(df_ter) <- names(df_ter) %>%
  str_replace_all("\\s*\\(.*?\\)", "") %>%
  str_trim()

# 4) Jahres‑Spalten gezielt festlegen
years     <- as.character(2005:2023)
year_cols <- intersect(names(df_ter), years)

# 5) **Alle** Jahres‑Spalten in Character umwandeln
df_ter <- df_ter %>%
  mutate(across(all_of(year_cols), as.character))

# 6) Pivot in long-Format über genau diese Jahre
df_ter_long <- df_ter %>%
  select(Land, all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "Jahr",
    values_to = "Tertiary",
    values_transform = list(Tertiary = as.character)
  )

# 7) Saubere Numeric‑Konvertierung und Region‑Zuordnung
df_ter_region <- df_ter_long %>%
  mutate(
    # Komma → Punkt, dann nur Ziffern und Punkt behalten
    ter_clean = str_replace_all(Tertiary, ",", ".") %>%
      str_remove_all("[^0-9\\.]"),
    Tertiary  = as.numeric(ter_clean),
    Jahr      = as.integer(Jahr),
    Region    = case_when(
      Land %in% old_states ~ "old",
      Land %in% new_states ~ "new",
      TRUE                ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region) & !is.na(Tertiary)) %>%
  group_by(Region, Jahr) %>%
  summarise(
    Tertiary = mean(Tertiary, na.rm = TRUE),
    .groups  = "drop"
  )

# 8) Pivot in wide-Format: eine Zeile pro Region, Jahre als Spalten
df_ter_wide <- df_ter_region %>%
  pivot_wider(
    names_from  = Jahr,
    values_from = Tertiary
  )

# 9) Ergebnis prüfen
print(df_ter_wide)

# 1) Einlesen ab der Zeile mit „Land,2005,…,2023“
df_BNE <- read_excel(
  "~/Desktop/Uni/Seminar/Daten/AK NE Nachhaltigkeitsindikatoren.xlsx",
  sheet     = "8_13",
  skip      = 3,
  col_names = TRUE
)

# 2) Erste Spalte auf „Land“ umbenennen
names(df_BNE)[1] <- "Land"

# 3) Fußnoten in Spaltennamen entfernen
names(df_BNE) <- names(df_BNE) %>%
  str_replace_all("\\s*\\(.*?\\)", "") %>%
  str_trim()

# 4) Jahres‑Spalten gezielt festlegen
years_BNE     <- as.character(1991:2021)
year_cols_BNE <- intersect(names(df_BNE), years_BNE)

# 5) **Alle** Jahres‑Spalten in Character umwandeln
df_BNE <- df_BNE %>%
  mutate(across(all_of(year_cols_BNE), as.character))

# 6) Pivot in long-Format über genau diese Jahre
df_BNE_long <- df_BNE%>%
  select(Land, all_of(year_cols_BNE)) %>%
  pivot_longer(
    cols      = all_of(year_cols_BNE),
    names_to  = "Jahr",
    values_to = "BNE",
    values_transform = list(Tertiary = as.character)
  )

# 7) Saubere Numeric‑Konvertierung und Region‑Zuordnung
df_BNE_region <- df_BNE_long %>%
  mutate(
    # Komma → Punkt, dann nur Ziffern und Punkt behalten
    BNE_clean = str_replace_all(BNE, ",", ".") %>%
      str_remove_all("[^0-9\\.]"),
    BNE  = as.numeric(BNE_clean),
    Jahr      = as.integer(Jahr),
    Region    = case_when(
      Land %in% old_states ~ "old",
      Land %in% new_states ~ "new",
      Land %in% "Deutschland" ~ "Deutschland",
      TRUE                ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region) & !is.na(BNE)) %>%
  group_by(Region, Jahr) %>%
  summarise(
    BNE = mean(BNE, na.rm = TRUE),
    .groups  = "drop"
  )

# 8) Pivot in wide-Format: eine Zeile pro Region, Jahre als Spalten
df_BNE_wide <- df_BNE_region %>%
  pivot_wider(
    id_cols     = Jahr,
    names_from  = Region,
    values_from = BNE,
    names_prefix= "BNE_"
  )

# 9) Ergebnis prüfen
print(df_BNE_wide)



# 1) Bildung long → wide für EarlyLeavers
df_el_wide <- df_el_region %>%
  pivot_wider(
    id_cols    = Jahr,
    names_from = Region,
    values_from= EarlyLeavers,
    names_prefix = "EarlyLeavers_"
  )
  df_el_wide<-  mutate(df_el_wide, "EarlyLeavers_Insgesamt" = as.numeric((df_el_wide$EarlyLeavers_new+df_el_wide$EarlyLeavers_old)/2))
# Ergebnis: Spalten Jahr, EarlyLeavers_old, EarlyLeavers_new

# 2) Bildung long → wide für Tertiary
df_ter_wide <- df_ter_region %>%
  pivot_wider(
    id_cols     = Jahr,
    names_from  = Region,
    values_from = Tertiary,
    names_prefix= "Tertiary_"
  )
df_ter_wide<- mutate(df_ter_wide, "Tertiary_Insgesammt" = as.numeric((df_ter_wide$Tertiary_new+df_ter_wide$Tertiary_old)/2))
# Ergebnis: Spalten Jahr, Tertiary_old, Tertiary_new

# 3) Alles in model_data joinen
model_data <- arbeitslosenquote %>%
  left_join(bip_group,         by = "Jahr") %>%
  left_join(df_Bruttoeinkommen,by = "Jahr") %>%
  left_join(df_el_wide,        by = "Jahr") %>%
  left_join(df_ter_wide,       by = "Jahr") %>%
  left_join(Private_Konsumausgaben, by = "Jahr") %>%
  left_join(Haushalts_nettoeinkommen, by = "Jahr") %>%
  left_join(df_BNE_wide, by ="Jahr")



# 4) Kontrolle
glimpse(model_data)
head(model_data)


### Deskriptive Analyse
model_data_deskStat<- model_data[,-1]
Durchschnitte<- colMeans(model_data_deskStat, na.rm = T)
# Annahme: deine Daten liegen im Objekt "Durchschnitte"

# Manuelle Zuordnung der Variablen aus dem Screenshot
variablen_liste <- list(
  "Arbeitslosenquote (%)" = c("Unemp_new", "Unemp_old"),
  "BIP ( Mio.€)" = c("GDP_new", "GDP_old"),
  "Bruttoeinkommen (€)" = c("Bruttoeinkommen_new", "Bruttoeinkommen_old"),
  "Frühschulabgänger (%)" = c("EarlyLeavers_new", "EarlyLeavers_old"),
  "Tertiärbildung (%)" = c("Tertiary_new", "Tertiary_old"),
  "Konsumausgaben (€)" = c("Konsumausgaben_new", "Konsumausgaben_old"),
  "Haushaltsnettoeinkommen (€)" = c("Haushaltsnetto_new", "Haushaltsnetto_old"),
  "BNE (Mio. €)" = c("BNE_new", "BNE_old")
)

# Leere Vektoren für die Tabelle
Variable <- c()
Mittelwert_Ost <- c()
Mittelwert_West <- c()
Differenz <- c()

# Durchlaufen der Liste und Einfügen in die Tabelle
for (var in names(variablen_liste)) {
  var_names <- variablen_liste[[var]]
  ost_wert <- if (!is.na(var_names[1])) Durchschnitte[var_names[1]] else NA
  west_wert <- if (!is.na(var_names[2])) Durchschnitte[var_names[2]] else NA
  diff_wert <- if (!is.na(ost_wert) & !is.na(west_wert)) ost_wert - west_wert else NA
  
  Variable <- c(Variable, var)
  Mittelwert_Ost <- c(Mittelwert_Ost, round(as.numeric(ost_wert), 1))
  Mittelwert_West <- c(Mittelwert_West, round(as.numeric(west_wert), 1))
  Differenz <- c(Differenz, round(as.numeric(diff_wert), 1))
}

# Tabelle zusammenfügen
vergleich <- data.frame(
  Variable = Variable,
  `Mittelwert (Ost)` = Mittelwert_Ost,
  `Mittelwert (West)` = Mittelwert_West,
  Differenz = Differenz,
  check.names = FALSE
)
vergleich<-
# Ausgabe der Tabelle
print(vergleich)



# Generiere LaTeX-Tabelle
latex_tabelle <- kable(vergleich, format = "latex", booktabs = TRUE, digits = 1,
                       caption = "Deskriptive Statistik zentraler Variablen (1995–2022)") %>%
  kable_styling(latex_options = c( "hold_position"))

# Speichere mit sink()
sink("Deskriptive_statistik.tex")
cat(latex_tabelle)
sink()

mod_old <- lm(Unemp_old ~ GDP_old + Bruttoeinkommen_old  + 
                EarlyLeavers_old+ Tertiary_old + Konsumausgaben_old + Haushaltsnetto_old + BNE_old ,data = model_data)
mod_new <- lm(Unemp_new ~ GDP_new + Bruttoeinkommen_new+  
                EarlyLeavers_new+ Tertiary_new + Konsumausgaben_new + Haushaltsnetto_new + BNE_new, data = model_data)

summary(mod_old)
summary(mod_new)


plot(mod_old)
plot(mod_new)


# tidy() fasst das Modell zusammen
tidy_old <- tidy(mod_old, conf.int = TRUE)
tidy_new <- tidy(mod_new, conf.int = TRUE)

# als LaTeX‑Tabelle ausgeben


# Signifikanzsterne definieren
tidy_old$Signif <- cut(tidy_old$p.value,
                  breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
                  labels = c("***", "**", "*", ""))

# Ergebnisformat anpassen
tidy_old$p.value <- sprintf("%.3f%s", tidy_old$p.value, tidy_old$Signif)

# Tabellenspalten benennen und auswählen
tidy_old <- tidy_old[, c("term", "estimate", "std.error", "statistic", "p.value")]
colnames(tidy_old) <- c("Regressor", "Schätzer", "Std. Fehler", "t-Wert", "p-Wert")
names(tidy_old) <- c("Regressor", "Schätzer", "Std. Fehler", "t-Wert", "p-Wert", "Konf.Intervall Lower Bound", "Konf.Intervall Upper Bound")
sink("mod_old.tex")
kable(tidy_old, format = "latex", booktabs = TRUE,
      caption = "Regressionsergebnisse für Ostdeutschland",
      digits = 3) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))
sink()

# Modell für Neue Bundelände
tidy_new$Signif <- cut(tidy_new$p.value,
                       breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
                       labels = c("***", "**", "*", ""))

# Ergebnisformat anpassen
tidy_new$p.value <- sprintf("%.3f%s", tidy_new$p.value, tidy_new$Signif)

# Tabellenspalten benennen und auswählen
tidy_new <- tidy_new[, c("term", "estimate", "std.error", "statistic", "p.value")]
colnames(tidy_old) <- c("Regressor", "Schätzer", "Std. Fehler", "t-Wert", "p-Wert")
sink("mod_new.tex")
kable(tidy_new, format = "latex", booktabs = TRUE,
      caption = "Regressionsergebnisse für Ostdeutschland",
      digits = 3) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))
sink()



### Pooled Regression 


# Long-Format erzeugen (nur Ost/West)
model_data_long <- model_data %>%
  select(
    Jahr,
    Unemp_old, Unemp_new,
    GDP_old, GDP_new,
    Bruttoeinkommen_old, Bruttoeinkommen_new,
    EarlyLeavers_old, EarlyLeavers_new,
    Tertiary_old, Tertiary_new,
    Konsumausgaben_old, Konsumausgaben_new,
    Haushaltsnetto_old, Haushaltsnetto_new,
    BNE_old, BNE_new
  ) %>%
  pivot_longer(
    cols = -Jahr,
    names_to = c("Variable", "Region"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = value
  )

# Region als Faktor setzen
model_data_long$Region <- factor(model_data_long$Region, levels = c("old", "new"))
# Pooled Regression mit Interaktion
mod_interaction <- lm(Unemp ~ 
                        GDP * Region +
                        Bruttoeinkommen * Region +
                        EarlyLeavers * Region +
                        Tertiary * Region +
                        Konsumausgaben * Region +
                        Haushaltsnetto * Region +
                        BNE * Region,
                      data = model_data_long)

summary(mod_interaction)



# Regressionskoeffizienten formatieren
reg_table <- tidy(mod_interaction)
names(reg_table)<- c("term", "estimate","std.error", "t_statistic" ,"p.value" )
# Sternchen für Signifikanz hinzufügen
reg_table$stars <- cut(reg_table$p.value,
                       breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
                       labels = c("***", "**", "*", ""))

# Rundung
reg_table <- reg_table %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    t_statistic = round(t_statistic, 3),
    p.value = round(p.value, 3)
  )

# Export als LaTeX
sink("mod_interaction.tex")
cat("\\begin{table}[H]\n\\centering\n")
cat("\\caption{Pooled Regression mit Interaktionseffekten}\n")
cat("\\label{tab:pooled_interaction}\n")
cat("\\begin{tabular}{lllll}\n")
cat("\\toprule\n")
cat("Regressor & Schätzer & Std. Fehler & t-Wert & p-Wert \\\\\n")
cat("\\midrule\n")

for (i in 1:nrow(reg_table)) {
  cat(paste0(
    reg_table$term[i], " & ",
    reg_table$estimate[i], " & ",
    reg_table$std.error[i], " & ",
    reg_table$t_statistic[i], " & ",
    reg_table$p.value[i], reg_table$stars[i], " \\\\\n"
  ))
}

cat("\\bottomrule\n\\end{tabular}\n\\end{table}\n")
sink()









# ────────────────────────────────────────────────────────────────────────────────
#  Multikollinearität & Heteroskedastizität & robuste SE + Durbin–Watson je Region
# ────────────────────────────────────────────────────────────────────────────────


# ---- VIF-Tabelle ----

vif_old<- vif(mod_old)
vif_new<- vif(mod_new)
# Hilfsfunktion zum Entfernen des Suffixes
strip_suffix <- function(v) {
  gsub("(_old|_new)", "", v)
}

# Vektoren vorbereiten
vars_old <- names(vif_old)
vars_new <- names(vif_new)
base_vars <- unique(c(strip_suffix(vars_old), strip_suffix(vars_new)))

# Umbenennung der Vektornamen
names(vif_old) <- strip_suffix(names(vif_old))
names(vif_new) <- strip_suffix(names(vif_new))

# Tabelle schreiben
sink("vif_tabelle.tex")
cat("\\begin{table}[H]\n\\centering\n")
cat("\\caption{VIF-Werte zur Diagnose von Multikollinearität – Vergleich Ost und West}\n")
cat("\\label{tab:vif}\n")
cat("\\begin{tabular}{lcc}\n")
cat("\\toprule\n")
cat("Regressor & Westdeutschland & Ostdeutschland \\\\\n")
cat("\\midrule\n")

for (var in base_vars) {
  val_west <- ifelse(var %in% names(vif_old), sprintf("%.2f", vif_old[var]), "")
  val_ost  <- ifelse(var %in% names(vif_new), sprintf("%.2f", vif_new[var]), "")
  cat(sprintf("%s & %s & %s \\\\\n", var, val_west, val_ost))
}

cat("\\bottomrule\n\\end{tabular}\n\\end{table}\n")
sink()


# ---- Breusch-Pagan-Tabelle ----
sink("bp_tabelle.tex")
cat("\\begin{table}[H]\n\\centering\n")
cat("\\caption{Breusch-Pagan-Test zur Diagnose von Heteroskedastizität}\n")
cat("\\label{tab:bp}\n")
cat("\\begin{tabular}{lcc}\n")
cat("\\toprule\n")
cat(" & BP-Teststatistik & p-Wert \\\\\n")
cat("\\midrule\n")
cat(paste0("Westdeutschland & ", round(bp_old$statistic, 3),
           " (df = ", bp_old$parameter, ") & ", signif(bp_old$p.value, 4), " \\\\\n"))
cat(paste0("Ostdeutschland & ", round(bp_new$statistic, 3),
           " (df = ", bp_new$parameter, ") & ", signif(bp_new$p.value, 4), " \\\\\n"))
cat("\\bottomrule\n\\end{tabular}\n\\end{table}\n")
sink()


# ---- Durbin-Watson-Tabelle ----
sink("dw_tabelle.tex")
cat("\\begin{table}[H]\n\\centering\n")
cat("\\caption{Durbin-Watson-Test zur Diagnose von Autokorrelation}\n")
cat("\\label{tab:dw}\n")
cat("\\begin{tabular}{lcc}\n")
cat("\\toprule\n")
cat(" & DW-Statistik & p-Wert \\\\\n")
cat("\\midrule\n")
cat(paste0("Westdeutschland & ", round(dw_old$statistic, 3),
           " & ", signif(dw_old$p.value, 4), " \\\\\n"))
cat(paste0("Ostdeutschland & ", round(dw_new$statistic, 3),
           " & ", signif(dw_new$p.value, 4), " \\\\\n"))
cat("\\bottomrule\n\\end{tabular}\n\\end{table}\n")
sink()


# Robuste Standardfehler
hc_old <- coeftest(mod_old, vcov = vcovHC(mod_old, type = "HC1"))
hc_new <- coeftest(mod_new, vcov = vcovHC(mod_new, type = "HC1"))

# Sternchen-Funktion
stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1)  return("*")
  return("")
}

# Funktion zur formatieren der einzelnen Werte
fmt <- function(x) {
  if (is.na(x)) return("NA")
  sprintf("%.3f", x)
}

# Funktion zum Zeilenbau
get_vals <- function(hc, var) {
  if (!(var %in% rownames(hc))) return(rep("NA", 4))
  est  <- fmt(hc[var, 1])
  se   <- fmt(hc[var, 2])
  tval <- fmt(hc[var, 3])
  pval <- sprintf("%.3f%s", hc[var, 4], stars(hc[var, 4]))
  return(c(est, se, tval, pval))
}

# Gemeinsame Variablennamen ohne _old/_new
all_vars <- union(rownames(hc_old), rownames(hc_new))
base_vars <- unique(gsub("(_old|_new)", "", all_vars))

# Tabelle schreiben
sink("hc1_tabelle_kurz.tex")
cat("\\begin{table}[H]\n\\centering\n")
cat("\\caption{Robuste Standardfehler nach White (HC1) – Vergleich Ost und West}\n")
cat("\\label{tab:hc1_kurz}\n")
cat("\\begin{tabular}{lcccc|cccc}\n")
cat("\\toprule\n")
cat(" & \\multicolumn{4}{c|}{Westdeutschland} & \\multicolumn{4}{c}{Ostdeutschland} \\\\\n")
cat("Variable & Schätzer & Std. Fehler & t-Wert & p-Wert & Schätzer & Std. Fehler & t-Wert & p-Wert \\\\\n")
cat("\\midrule\n")

for (var in base_vars) {
  name_old <- ifelse(var == "(Intercept)", var, paste0(var, "_old"))
  name_new <- ifelse(var == "(Intercept)", var, paste0(var, "_new"))
  vals_old <- get_vals(hc_old, name_old)
  vals_new <- get_vals(hc_new, name_new)
  cat(sprintf("%s & %s \\\\\n", var, paste(c(vals_old, vals_new), collapse = " & ")))
}

cat("\\bottomrule\n\\end{tabular}\n\\end{table}\n")
sink()







# ────────────────────────────────────────────────────────────────────────────────
#  Rolling‑Regression (10‑Jahres‑Fenster) je Region
# ────────────────────────────────────────────────────────────────────────────────
if (!require(zoo)) install.packages("zoo"); library(zoo)

roll_coef <- function(y, x, df, width = 10) {
  rollapply(df, width, by.column = FALSE,
            FUN = function(d) {
              d <- as.data.frame(d)  # aus Matrix wieder DataFrame machen
              coef(lm(d[[y]] ~ d[[x]]))[2]
            }
  )
}


# Einkommenseffekt rollierend
rc_old <- roll_coef("Unemp_old","Bruttoeinkommen_old", model_data)
rc_new <- roll_coef("Unemp_new","Bruttoeinkommen_new", model_data)

years_roll <- model_data$Jahr[seq(length(rc_old))] + (10-1)
plot(years_roll, rc_old, type = "l", ylim = range(c(rc_old,rc_new), na.rm=TRUE),
     xlab="Jahr (Fensterende)", ylab="Coeff Einkommen", main="Rolling Einkommens Effekt", col="blue")
grid()
lines(years_roll, rc_new, lty=2, col="red")
legend("bottomright", c("alt","neu"), lty=1:2, cex=.8, col=c("blue", "red"))


### Potenzieller Skatterplot

# Daten vorbereiten
plot_data <- model_data %>%
  select(Jahr, GDP_old, GDP_new, Unemp_old, Unemp_new) %>%
  pivot_longer(
    cols = c(GDP_old, GDP_new, Unemp_old, Unemp_new),
    names_to = c(".value", "Region"),
    names_pattern = "(.*)_(old|new)"
  ) %>%
  mutate(Region = ifelse(Region == "old", "Westdeutschland", "Ostdeutschland"))

# Scatterplot mit Smoothing-Linien
ggplot(plot_data, aes(x = GDP, y = Unemp, color = Region, shape = Region)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 1.1) +
  labs(
    title = "Zusammenhang zwischen BIP (in Mio. €) 
    und Arbeitslosenquote",
    subtitle = "Getrennt nach alten und neuen Bundesländern (1995–2024)", 
    x = "BIP (in Mio. €)",
    y = "Arbeitslosenquote (%)",
    color = "Region",
    shape = "Region"
  ) +
  scale_color_manual(values = c("Ostdeutschland" = "steelblue", "Westdeutschland" = "darkred")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )






