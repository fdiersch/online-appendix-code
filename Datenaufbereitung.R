###############################################################################
# Skript: Datenaufbereitung für konfirmatorische Faktorenanalyse (CFA) und 
# Korrelationsanalysen (FREE × externe Skalen)
#
# Ziel:
#   (1) Erstellen der Subsets mit den FREE-Items.
#   (2) Erstellung eines zusammengeführten Datensatzes zur Berechnung
#       der Korrelationen zwischen den FREE-Skalen (Absolutismus, Relativismus,
#       Postrelativismus) und externen Skalen (EBI-AM, LIST-K, BilWiss) mit
#       Skalenmittelwerten.
#   (3) Erstellen eines separaten Datensatzes für die Korrelationen von FEE
#       und FREE (Skalen- und Itemebene).
#
# Datengrundlage: 
#   - free_only_t0.RData  (frühere Erhebung, nur FREE erhoben)
#   - fee_data.RData      (Teilstichprobe, in der FREE und FEE erhoben wurden)
#   - ew_t1.RData, la_t1.RData  (Teilerhebungen 1: FREE, LIST-K, EBI-AM, BilWiss)
#   - ew_t2.RData, la_t2.RData  (Teilerhebungen 2: FREE, LIST-K, EBI-AM, BilWiss)
###############################################################################

library(dplyr)

###############################################################################
# 1. FREE-Subsets für CFA / Reliabilitäten
###############################################################################
# Bereinigte Einzeldatensätze laden
load("free_only_t0.RData")  # frühere Erhebung, nur FREE
load("fee_data.RData")      # Teilstichprobe mit FREE + FEE
load("ew_t1.RData")         # EW-Stichprobe, T1
load("la_t1.RData")         # LA-Stichprobe, T1
load("ew_t2.RData")         # EW-Stichprobe, T2
load("la_t2.RData")         # LA-Stichprobe, T2

## Ziel: FREE-Subsets aus den Einzeldatensätzen generieren --------------------

## Ziel: FREE-Subsets aus den Einzeldatensätzen generieren --------------------

# frühere Erhebung, nur FREE
free_t0 <- subset(
  free_only_t0,
  select = c(
    Ab01, Ab02, Ab03, Ab04, Ab05, Ab06, Ab07, Ab08, Ab09, Ab10, Ab11, Ab12, Ab13,
    Rel01, Rel02, Rel03, Rel04, Rel05, Rel06, Rel07, Rel08, Rel09, Rel10, Rel11, Rel12, Rel13,
    Pos01, Pos02, Pos03, Pos04, Pos05, Pos06, Pos07, Pos08, Pos09, Pos10, Pos11, Pos12, Pos13
  )
)

# Teilstichprobe mit FREE + FEE
fee_free <- subset(
  fee_data,
  select = c(
    CASE, sex, age, study,
    Ab01, Ab02, Ab03, Ab04, Ab05, Ab06, Ab07, Ab08, Ab09, Ab10, Ab11, Ab12, Ab13,
    Rel01, Rel02, Rel03, Rel04, Rel05, Rel06, Rel07, Rel08, Rel09, Rel10, Rel11, Rel12, Rel13,
    Pos01, Pos02, Pos03, Pos04, Pos05, Pos06, Pos07, Pos08, Pos09, Pos10, Pos11, Pos12, Pos13
  )
)

# EW / LA, T1: FREE + Validierungsskalen
ew_free_t1 <- subset(
  ew_t1,
  select = c(
    CASE, sex, age, study,
    Ab01, Ab02, Ab03, Ab04, Ab05, Ab06, Ab07, Ab08, Ab09, Ab10, Ab11, Ab12, Ab13,
    Rel01, Rel02, Rel03, Rel04, Rel05, Rel06, Rel07, Rel08, Rel09, Rel10, Rel11, Rel12, Rel13,
    Pos01, Pos02, Pos03, Pos04, Pos05, Pos06, Pos07, Pos08, Pos09, Pos10, Pos11, Pos12, Pos13
  )
)

la_free_t1 <- subset(
  la_t1,
  select = c(
    CASE, sex, age, study,
    Ab01, Ab02, Ab03, Ab04, Ab05, Ab06, Ab07, Ab08, Ab09, Ab10, Ab11, Ab12, Ab13,
    Rel01, Rel02, Rel03, Rel04, Rel05, Rel06, Rel07, Rel08, Rel09, Rel10, Rel11, Rel12, Rel13,
    Pos01, Pos02, Pos03, Pos04, Pos05, Pos06, Pos07, Pos08, Pos09, Pos10, Pos11, Pos12, Pos13
  )
)

# EW / LA, T2: FREE + Validierungsskalen
# Hinweis: In EW T2 fehlt Item 9 (Ab09/Rel09/Pos09), da im Fragebogen fälschlich ME19 verwendet wurde.
ew_free_t2 <- subset(
  ew_t2,
  select = c(
    CASE, sex, age, study,
    Ab01, Ab02, Ab03, Ab04, Ab05, Ab06, Ab07, Ab08, Ab10, Ab11, Ab12, Ab13,
    Rel01, Rel02, Rel03, Rel04, Rel05, Rel06, Rel07, Rel08, Rel10, Rel11, Rel12, Rel13,
    Pos01, Pos02, Pos03, Pos04, Pos05, Pos06, Pos07, Pos08, Pos10, Pos11, Pos12, Pos13
  )
)

la_free_t2 <- subset(
  la_t2,
  select = c(
    CASE, sex, age, study,
    Ab01, Ab02, Ab03, Ab04, Ab05, Ab06, Ab07, Ab08, Ab09, Ab10, Ab11, Ab12, Ab13,
    Rel01, Rel02, Rel03, Rel04, Rel05, Rel06, Rel07, Rel08, Rel09, Rel10, Rel11, Rel12, Rel13,
    Pos01, Pos02, Pos03, Pos04, Pos05, Pos06, Pos07, Pos08, Pos09, Pos10, Pos11, Pos12, Pos13
  )
)

# FREE-Subsets sichern (für CFA / Reliabilitäten)
save(
  free_t0, fee_free, ew_free_t1, la_free_t1, ew_free_t2, la_free_t2,
  file = "Subsets_FREE_all.RData"
)

# Gemeinsamer FREE-Datensatz für CFA / Reliabilität
free_subs_all <- bind_rows(
  free_t0,
  fee_free,
  ew_free_t1, la_free_t1,
  ew_free_t2, la_free_t2
)

# numerische Umformung
free_subs_all <- free_subs_all %>%
  mutate(across(everything(), as.numeric, na.rm = TRUE))

# -9 in NA umkodieren
free_subs_all <- free_subs_all %>%
  mutate(across(everything(), ~ na_if(., -9)))

# Auswahl: Fälle mit mindestens einem gültigen FREE-Item
me_items <- c(
  "Ab01","Ab02","Ab03","Ab04","Ab05","Ab06","Ab07","Ab08","Ab09",
  "Ab10","Ab11","Ab12","Ab13",
  "Rel01","Rel02","Rel03","Rel04","Rel05","Rel06","Rel07","Rel08","Rel09",
  "Rel10","Rel11","Rel12","Rel13",
  "Pos01","Pos02","Pos03","Pos04","Pos05","Pos06","Pos07","Pos08","Pos09",
  "Pos10","Pos11","Pos12","Pos13"
)

free_subs_all <- free_subs_all %>%
  rowwise() %>%
  filter(sum(!is.na(c_across(all_of(me_items)))) >= 1) %>%
  ungroup()

# Hinweis:
# free_subs_all ist der endgültige Datensatz für CFA und Reliabilitätsanalysen
# der FREE-Skalen (Absolutismus, Relativismus, Postrelativismus).

###############################################################################
# 2. Subsets der Validierungsskalen (EBI-AM, LIST-K, BilWiss)
#    für Korrelationsanalysen mit FREE
###############################################################################

### 2.1 EBI-AM-Subsets --------------------------------------------------------

# Erziehungswissenschafts-Studierende, Erhebung 1
ew_ebi_t1 <- subset(
  ew_t1,
  select = c(
    CASE, sex, age, study,
    EB01_01, EB01_02, EB01_03, EB01_04, EB01_05, EB01_06,
    EB01_07, EB01_08, EB01_09, EB01_10, EB01_11,
    EB01_12, EB01_13, EB01_14, EB01_15, EB01_16,
    EB01_17, EB01_18, EB01_19, EB01_20, EB01_21, EB01_22, EB01_23
  )
)
# Erziehungswissenschafts-Studierende, Erhebung 2
ew_ebi_t2 <- subset(
  ew_t2,
  select = c(
    CASE, sex, age, study,
    EB01_01, EB01_02, EB01_03, EB01_04, EB01_05, EB01_06,
    EB01_07, EB01_08, EB01_09, EB01_10, EB01_11,
    EB01_12, EB01_13, EB01_14, EB01_15, EB01_16,
    EB01_17, EB01_18, EB01_19, EB01_20, EB01_21, EB01_22, EB01_23
  )
)
# Lehramts-Studierende, Erhebung 1
la_ebi_t1 <- subset(
  la_t1,
  select = c(
    CASE, sex, age, study,
    EB01_01, EB01_02, EB01_03, EB01_04, EB01_05, EB01_06,
    EB01_07, EB01_08, EB01_09, EB01_10, EB01_11,
    EB01_12, EB01_13, EB01_14, EB01_15, EB01_16,
    EB01_17, EB01_18, EB01_19, EB01_20, EB01_21, EB01_22, EB01_23
  )
)
# Lehramts-Studierende, Erhebung 2
la_ebi_t2 <- subset(
  la_t2,
  select = c(
    CASE, sex, age, study,
    EB01_01, EB01_02, EB01_03, EB01_04, EB01_05, EB01_06,
    EB01_07, EB01_08, EB01_09, EB01_10, EB01_11,
    EB01_12, EB01_13, EB01_14, EB01_15, EB01_16,
    EB01_17, EB01_18, EB01_19, EB01_20, EB01_21, EB01_22, EB01_23
  )
)

### 2.2 LIST-K-Subsets --------------------------------------------------------

# Erziehungswissenschafts-Studierende, Erhebung 1
ew_list_t1 <- subset(
  ew_t1,
  select = c(
    CASE, sex, age, study,
    LI01_01, LI01_02, LI01_03,
    LI01_04, LI01_05, LI01_06,
    LI01_07, LI01_08, LI01_09,
    LI01_10, LI01_11, LI01_12
  )
)
# Erziehungswissenschafts-Studierende, Erhebung 2
ew_list_t2 <- subset(
  ew_t2,
  select = c(
    CASE, sex, age, study,
    LI01_01, LI01_02, LI01_03,
    LI01_04, LI01_05, LI01_06,
    LI01_07, LI01_08, LI01_09,
    LI01_10, LI01_11, LI01_12
  )
)

# Lehramts-Studierende, Erhebung 1
la_list_t1 <- subset(
  la_t1,
  select = c(
    CASE, sex, age, study,
    LI01_01, LI01_02, LI01_03,
    LI01_04, LI01_05, LI01_06,
    LI01_07, LI01_08, LI01_09,
    LI01_10, LI01_11, LI01_12
  )
)
# Lehramts-Studierende, Erhebung 2
la_list_t2 <- subset(
  la_t2,
  select = c(
    CASE, sex, age, study,
    LI01_01, LI01_02, LI01_03,
    LI01_04, LI01_05, LI01_06,
    LI01_07, LI01_08, LI01_09,
    LI01_10, LI01_11, LI01_12
  )
)


### 2.3 BilWiss-Subsets --------------------------------------------------------

# Erziehungswissenschafts-Studierende, Erhebung 1
ew_bw_t1 <- subset(
  ew_t1,
  select = c(
    CASE, sex, age, study,
    BW01_01, BW01_02, BW01_03, BW01_04,
    BW02_01, BW02_02, BW02_03, BW02_04,
    BW03,
    BW04_01, BW04_02, BW04_03, BW04_04, BW05,
    BW06_01, BW06_02, BW06_03, BW06_04,
    BW07_01, BW07_02, BW07_03, BW07_04,
    BW08_01, BW08_02, BW08_03, BW08_04,
    BW09_01, BW09_02, BW09_03, BW09_04, BW10,
    BW11_01, BW11_02, BW11_03, BW11_04,
    BW12_01, BW12_02, BW12_03, BW12_04, BW13,
    BW14_01, BW14_02, BW14_03, BW14_04,
    BW15_01, BW15_02, BW15_03, BW15_04, BW16, BW17,
    BW18_01, BW18_02, BW18_03, BW18_04,
    BW19_01, BW19_02, BW19_03, BW19_04,
    BW20_01, BW20_02, BW20_03, BW20_04,
    BW21_01, BW21_02, BW21_03, BW21_04, BW22
  )
)
# Erziehungswissenschafts-Studierende, Erhebung 2
ew_bw_t2 <- subset(
  ew_t2,
  select = c(
    CASE, sex, age, study,
    BW01_01, BW01_02, BW01_03, BW01_04,
    BW02_01, BW02_02, BW02_03, BW02_04,
    BW03,
    BW04_01, BW04_02, BW04_03, BW04_04, BW05,
    BW06_01, BW06_02, BW06_03, BW06_04,
    BW07_01, BW07_02, BW07_03, BW07_04,
    BW08_01, BW08_02, BW08_03, BW08_04,
    BW09_01, BW09_02, BW09_03, BW09_04, BW10,
    BW11_01, BW11_02, BW11_03, BW11_04,
    BW12_01, BW12_02, BW12_03, BW12_04, BW13,
    BW14_01, BW14_02, BW14_03, BW14_04,
    BW15_01, BW15_02, BW15_03, BW15_04, BW16, BW17,
    BW18_01, BW18_02, BW18_03, BW18_04,
    BW19_01, BW19_02, BW19_03, BW19_04,
    BW20_01, BW20_02, BW20_03, BW20_04,
    BW21_01, BW21_02, BW21_03, BW21_04, BW22
  )
)
# Lehramts-Studierende, Erhebung 1
la_bw_t1 <- subset(
  la_t1,
  select = c(
    CASE, sex, age, study,
    BW01_01, BW01_02, BW01_03, BW01_04,
    BW02_01, BW02_02, BW02_03, BW02_04,
    BW03,
    BW04_01, BW04_02, BW04_03, BW04_04, BW05,
    BW06_01, BW06_02, BW06_03, BW06_04,
    BW07_01, BW07_02, BW07_03, BW07_04,
    BW08_01, BW08_02, BW08_03, BW08_04,
    BW09_01, BW09_02, BW09_03, BW09_04, BW10,
    BW11_01, BW11_02, BW11_03, BW11_04,
    BW12_01, BW12_02, BW12_03, BW12_04, BW13,
    BW14_01, BW14_02, BW14_03, BW14_04,
    BW15_01, BW15_02, BW15_03, BW15_04, BW16, BW17,
    BW18_01, BW18_02, BW18_03, BW18_04,
    BW19_01, BW19_02, BW19_03, BW19_04,
    BW20_01, BW20_02, BW20_03, BW20_04,
    BW21_01, BW21_02, BW21_03, BW21_04, BW22
  )
)

# Lehramts-Studierende, Erhebung 2
la_bw_t2 <- subset(
  la_t2,
  select = c(
    CASE, sex, age, study,
    BW01_01, BW01_02, BW01_03, BW01_04,
    BW02_01, BW02_02, BW02_03, BW02_04,
    BW03,
    BW04_01, BW04_02, BW04_03, BW04_04, BW05,
    BW06_01, BW06_02, BW06_03, BW06_04,
    BW07_01, BW07_02, BW07_03, BW07_04,
    BW08_01, BW08_02, BW08_03, BW08_04,
    BW09_01, BW09_02, BW09_03, BW09_04, BW10,
    BW11_01, BW11_02, BW11_03, BW11_04,
    BW12_01, BW12_02, BW12_03, BW12_04, BW13,
    BW14_01, BW14_02, BW14_03, BW14_04,
    BW15_01, BW15_02, BW15_03, BW15_04, BW16, BW17,
    BW18_01, BW18_02, BW18_03, BW18_04,
    BW19_01, BW19_02, BW19_03, BW19_04,
    BW20_01, BW20_02, BW20_03, BW20_04,
    BW21_01, BW21_02, BW21_03, BW21_04, BW22
  )
)

###############################################################################
# 3. Zusammenführen der Subsets zu einem Korrelationsdatensatz (all_corr)
###############################################################################
# Konzept:
#   - Pro Erhebungsgruppe (EW1, LA1, LA2) werden EBI, LIST-K, FREE/MERK
#     und BilWiss zusammengeführt (linke Joins,
#     da pro Stichprobe identische Fälle angenommen werden).
#   - Anschließend werden die drei Erhebungsgruppen zusammengefügt
#   - FEE bleibt separat und wird später nur für FREE × FEE-Korrelationen genutzt.


### 3.1 Kontrolle auf doppelte CASE-Nummern ------------------------

# keine Dopplungen enthalten

### FREE
# EW
ew_free_t1$CASE  %in% ew_free_t2$CASE
#LA
la_free_t1$CASE  %in% la_free_t2$CASE

### EBI
# EW
ew_ebi_t1$CASE %in% ew_ebi_t2$CASE
# LA
la_ebi_t1$CASE %in% la_ebi_t2$CASE

### LIST
# EW
ew_list_t1$CASE %in% ew_list_t2$CASE
# LA
la_list_t1$CASE %in% la_list_t2$CASE

### BilWiss
# EW
ew_bw_t1$CASE %in% ew_bw_t2$CASE
# LA
la_bw_t1$CASE %in% la_bw_t2$CASE

### 3.2 Zusammenführung pro Erhebungsgruppe -----------------------------------

# Erziehungswissenschafts-Studierende, Erhebung 1 (EW1):
# EBI + LIST-K + FREE/MERK + BilWiss

EW1.a <- left_join(ew_ebi_t1,ew_list_t1,by = "CASE")
EW1.b <- left_join(EW1.a,ew_free_t1,by = "CASE")
EW1.c <- left_join(EW1.b,ew_bw_t1,by = "CASE")

# ID-Spalte zur eindeutigen Identifikation innerhalb der Erhebung
EW1.c$ID <- 1:nrow(EW1.c)

# (EW2 wird hier bewusst nicht in all_corr integriert, nur 5 Cases, unterschiedliche Variablen im Datensatz)

# Lehramtsstudierende, Erhebung 1 (LA1)

LA1.a <- left_join(la_ebi_t1,la_list_t1,by = "CASE")
LA1.b <- left_join(LA1.a,la_free_t1, by = "CASE")
LA1.c <- left_join(LA1.b,la_bw_t1,by = "CASE")

LA1.c$ID <- 1:nrow(LA1.c)


# Lehramtsstudierende, Erhebung 2 (LA2)

LA2.a <- left_join(la_ebi_t2,la_list_t2,by = "CASE")
LA2.b <- left_join(LA2.a,la_free_t2,by = "CASE")
LA2.c <- left_join(LA2.b,la_bw_t2,by = "CASE")

LA2.c$ID <- 1:nrow(LA2.c)


### 3.3 Zusammenführung der drei Erhebungsgruppen zu all_corr -----------------

# Hinweis:
# Für das Zusammenführen der einzelnen Erhebungsgruppen (EW1, LA1, LA2) wird 
# bind_rows() verwendet, vorherige Prüfung auf doppelte CASE-Nummern stellt sicher, 
# dass jede Person genau einmal im Gesamtdatensatz enthalten ist.
# Datensätze sollen nicht zeilenweise gematcht werden, 
# alle Fälle werden untereinander angehängt ("unten drangeklebt").
# Grund: Jede Person hat einen eindeutigen CASE, der sich zwischen Gruppen nicht überschneidet.
# Alle Datensätze besitzen identische Spalten, daher werden die Werte korrekt ausgerichtet
# und fehlende Antworten bleiben als NA erhalten (keine Verschiebung der Itemwerte).

# Alle Teildatensätze (EW1, LA1, LA2) zusammenführen
all_corr <- dplyr::bind_rows(EW1.c, LA1.c, LA2.c)

# Eindeutige ID über alle zusammengeführten Fälle hinweg
all_corr$overall_id <- seq_len(nrow(all_corr))

# Zwischenstand speichern: Basisdatensatz für Korrelationsanalysen
save(all_corr, file = "all_corr.RData")


###############################################################################
# 4. Skalenmittelwerte für FREE, EBI-AM und LIST-K berechnen
###############################################################################

### 4.1 FREE-Skalen (Absolutismus, Relativismus, Postrelativismus) ------------

# Mindestkriterium: mindestens 12 von 13 beantwortete Items pro Skala.
# Skalenmittelwerte werden auf drei Nachkommastellen gerundet.

# Absolutismus (Ab01–Ab13)
mean_abs <- all_corr %>%
  rowwise() %>%
  mutate(
    answered_abs = sum(!is.na(c_across(Ab01:Ab13))),
    mean_abs = ifelse(
      answered_abs >= 12,
      mean(c_across(Ab01:Ab13), na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(overall_id, mean_abs)

mean_abs$mean_abs <- round(mean_abs$mean_abs, 3)

all_corr <- merge(mean_abs, all_corr, by = "overall_id")


# Relativismus (Rel01–Rel13)
mean_rel <- all_corr %>%
  rowwise() %>%
  mutate(
    answered_rel = sum(!is.na(c_across(Rel01:Rel13))),
    mean_rel = ifelse(
      answered_rel >= 12,
      mean(c_across(Rel01:Rel13), na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(overall_id, mean_rel)

mean_rel$mean_rel <- round(mean_rel$mean_rel, 3)

all_corr <- merge(mean_rel, all_corr, by = "overall_id")


# Postrelativismus (Pos01–Pos13)
mean_pos <- all_corr %>%
  rowwise() %>%
  mutate(
    answered_pos = sum(!is.na(c_across(Pos01:Pos13))),
    mean_pos = ifelse(
      answered_pos >= 12,
      mean(c_across(Pos01:Pos13), na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(overall_id, mean_pos)

mean_pos$mean_pos <- round(mean_pos$mean_pos, 3)

all_corr <- merge(mean_pos, all_corr, by = "overall_id")


### 4.2 Skalenmittelwerte EBI-AM ----------------------------------------------

# Multiplismus (EBI-AM)
mean_mult <- all_corr %>%
  group_by(overall_id) %>%
  summarise(
    mean_mult = mean(
      c(
        EB01_01, EB01_02, EB01_03, EB01_04, EB01_05,
        EB01_06, EB01_07, EB01_08, EB01_09, EB01_10,
        EB01_11
      ),
      na.rm = TRUE
    )
  )

mean_mult$mean_mult <- round(mean_mult$mean_mult, 3)
all_corr <- merge(mean_mult, all_corr, by = "overall_id")


# Absolutismus (EBI-AM)
mean_abs_ebi <- all_corr %>%
  group_by(overall_id) %>%
  summarise(
    mean_abs_ebi = mean(
      c(
        EB01_12, EB01_13, EB01_14, EB01_15,
        EB01_16, EB01_17, EB01_18, EB01_19,
        EB01_20, EB01_21, EB01_22, EB01_23
      ),
      na.rm = TRUE
    )
  )

mean_abs_ebi$mean_abs_ebi <- round(mean_abs_ebi$mean_abs_ebi, 3)
all_corr <- merge(mean_abs_ebi, all_corr, by = "overall_id")


### 4.3 Skalenmittelwerte LIST-K ----------------------------------------------

# Elaboration
mean_elab <- all_corr %>%
  group_by(overall_id) %>%
  summarise(
    mean_elab = mean(c(LI01_01, LI01_02, LI01_03), na.rm = TRUE)
  )

mean_elab$mean_elab <- round(mean_elab$mean_elab, 3)
all_corr <- merge(mean_elab, all_corr, by = "overall_id")


# Kritisches Prüfen
mean_crit <- all_corr %>%
  group_by(overall_id) %>%
  summarise(
    mean_crit = mean(c(LI01_04, LI01_05, LI01_06), na.rm = TRUE)
  )

mean_crit$mean_crit <- round(mean_crit$mean_crit, 3)
all_corr <- merge(mean_crit, all_corr, by = "overall_id")


# Wiederholen
mean_rep <- all_corr %>%
  group_by(overall_id) %>%
  summarise(
    mean_rep = mean(c(LI01_07, LI01_08, LI01_09), na.rm = TRUE)
  )

mean_rep$mean_rep <- round(mean_rep$mean_rep, 3)
all_corr <- merge(mean_rep, all_corr, by = "overall_id")


# Regulieren
mean_reg <- all_corr %>%
  group_by(overall_id) %>%
  summarise(
    mean_reg = mean(c(LI01_10, LI01_11, LI01_12), na.rm = TRUE)
  )

mean_reg$mean_reg <- round(mean_reg$mean_reg, 3)
all_corr <- merge(mean_reg, all_corr, by = "overall_id")


# Finalen Datensatz für Korrelationsanalysen (FREE × EBI-AM × LIST-K) speichern
save(all_corr, file = "all_corr_with_scalemeans.RData")


###############################################################################
# 5. Skalenmittelwerte FEE in der FEE-Teilstichprobe
###############################################################################

load("fee_data.RData")

# Hinweis:
# fee_data ist ein separater Datensatz, der die Teilstichprobe mit
# FREE- und FEE-Items enthält. Hier werden Skalenmittelwerte für die FEE-
# Subskalen und die FREE-Skalen berechnet, um FREE × FEE-Korrelationen
# auf Basis dieser Teilstichprobe zu bestimmen.

# FEE-Skalenmittelwerte und FREE Mittelwerte
# FEE-Skalenmittelwerte
fee_free_means <- fee_data %>%
  mutate(
    # FEE: Sicherheit von Wissen
    mean_abs_fee = rowMeans(
      select(., FE01_AB, FE02_AB, FE03_AB, FE04_AB, FE05_AB, FE06_AB, FE07_AB),
      na.rm = TRUE
    ),
    # FEE: Umgang mit Autoritäten
    mean_rel_fee = rowMeans(
      select(., FE08_RE, FE09_RE, FE10_RE, FE11_RE, FE12_RE, FE13_RE),
      na.rm = TRUE
    ),
    # FEE: Reflexive Natur von Wissen
    mean_pos_fee = rowMeans(
      select(., FE14_PO, FE15_PO, FE16_PO, FE17_PO, FE18_PO),
      na.rm = TRUE
    ),
    
    # FREE-Skalenmittelwerte (Ab, Rel, Pos) in der FEE-Teilstichprobe
    mean_abs = rowMeans(
      select(., Ab01, Ab02, Ab03, Ab04, Ab05, Ab06, Ab07,
             Ab08, Ab10, Ab11, Ab12, Ab13),
      na.rm = TRUE
    ),
    mean_rel = rowMeans(
      select(., Rel01, Rel02, Rel03, Rel04, Rel05, Rel06,
             Rel07, Rel08, Rel10, Rel11, Rel12, Rel13),
      na.rm = TRUE
    ),
    mean_pos = rowMeans(
      select(., Pos01, Pos02, Pos03, Pos04, Pos05, Pos06,
             Pos07, Pos08, Pos10, Pos11, Pos12, Pos13),
      na.rm = TRUE
    )
  )


# FEE-Teilstichprobe mit Skalenmittelwerten speichern
save(fee_free_means, file = "fee_free_means.RData")

###############################################################################
# Ende des Datenaufbereitungsskripts 
###############################################################################