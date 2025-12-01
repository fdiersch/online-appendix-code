###############################################################################
# Skript: Auswertung des BilWiss-Tests und Korrelationen mit FREE/MERK
#
# Inhalt:
#   (1) Scoring des BilWiss-Tests (Lernen & Entwicklung, Diagnostik, Gesamt)
#   (2) Korrelationen der BilWiss-Skalen mit den FREE-Skalen
#       (Absolutismus, Relativismus, Postrelativismus)
#   (3) Korrelationen der BilWiss-Skalen mit allen MERK-Items
#       (AB01–AB13, Rel01–Rel13, Pos01–Pos13)
#
# Voraussetzungen:
#   - Datenaufbereitungsskript ausgeführt
#
# Datengrundlage: 
#   - all.corr_with_scalemeans.RData (FREE, EBI, LIST, BilWiss inkl. Skalenmittelwerte)
###############################################################################

# Pakete laden ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(apaTables)
library(openxlsx)

# Datensatz laden -------------------------------------------------------------
load("all_corr_with_scalemeans.RData") #lädt "all_corr"

# Relevante BilWiss-Daten extrahieren (nur vollständige BilWiss-Fälle)
bilwiss_data <- all_corr_with_scalemeans %>%
  select(overall_id, starts_with("BW")) %>%
  drop_na()

###############################################################################
# 1. Scoring des BilWiss-Tests
###############################################################################

# 1.1 Rekodierung der Single-Choice-Aufgaben (SC) auf 0/1 ---------------------

# Richtige Antwort = 1, sonst 0 (gemäß Codebook)
bilwiss_data <- bilwiss_data %>%
  mutate(
    BW03 = ifelse(BW03 == 2, 1, 0),
    BW05 = ifelse(BW05 == 4, 1, 0),
    BW10 = ifelse(BW10 == 1, 1, 0),
    BW13 = ifelse(BW13 == 3, 1, 0),
    BW16 = ifelse(BW16 == 1, 1, 0),
    BW17 = ifelse(BW17 == 2, 1, 0),
    BW22 = ifelse(BW22 == 3, 1, 0)
  )

# Punkte aus SC-Aufgaben (0/2 Punkte pro Item)
bilwiss_data <- bilwiss_data %>%
  mutate(
    Punkte_BW03 = ifelse(BW03 == 1, 2, 0),
    Punkte_BW05 = ifelse(BW05 == 1, 2, 0),
    Punkte_BW10 = ifelse(BW10 == 1, 2, 0),
    Punkte_BW13 = ifelse(BW13 == 1, 2, 0),
    Punkte_BW16 = ifelse(BW16 == 1, 2, 0),
    Punkte_BW17 = ifelse(BW17 == 1, 2, 0),
    Punkte_BW22 = ifelse(BW22 == 1, 2, 0)
  )

# 1.2 Rekodierung der Multiple-Choice-Aufgaben (MC) auf 0/1 -------------------

# Richtige Antwortalternative = 1, falsche = 0 (gemäß Codebook)
bilwiss_data <- bilwiss_data %>%
  mutate(
    BW01_01 = recode(BW01_01, `1` = 0, `2` = 1),
    BW01_02 = recode(BW01_02, `1` = 1, `2` = 0),
    BW01_03 = recode(BW01_03, `1` = 0, `2` = 1),
    BW01_04 = recode(BW01_04, `1` = 0, `2` = 1),
    
    BW02_01 = recode(BW02_01, `1` = 1, `2` = 0),
    BW02_02 = recode(BW02_02, `1` = 1, `2` = 0),
    BW02_03 = recode(BW02_03, `1` = 0, `2` = 1),
    BW02_04 = recode(BW02_04, `1` = 1, `2` = 0),
    
    BW04_01 = recode(BW04_01, `1` = 1, `2` = 0),
    BW04_02 = recode(BW04_02, `1` = 0, `2` = 1),
    BW04_03 = recode(BW04_03, `1` = 1, `2` = 0),
    BW04_04 = recode(BW04_04, `1` = 0, `2` = 1),
    
    BW06_01 = recode(BW06_01, `1` = 0, `2` = 1),
    BW06_02 = recode(BW06_02, `1` = 1, `2` = 0),
    BW06_03 = recode(BW06_03, `1` = 0, `2` = 1),
    BW06_04 = recode(BW06_04, `1` = 1, `2` = 0),
    
    BW11_01 = recode(BW11_01, `1` = 0, `2` = 1),
    BW11_02 = recode(BW11_02, `1` = 0, `2` = 1),
    BW11_03 = recode(BW11_03, `1` = 1, `2` = 0),
    BW11_04 = recode(BW11_04, `1` = 1, `2` = 0),
    
    BW12_01 = recode(BW12_01, `1` = 1, `2` = 0),
    BW12_02 = recode(BW12_02, `1` = 0, `2` = 1),
    BW12_03 = recode(BW12_03, `1` = 0, `2` = 1),
    BW12_04 = recode(BW12_04, `1` = 1, `2` = 0),
    
    BW14_01 = recode(BW14_01, `1` = 1, `2` = 0),
    BW14_02 = recode(BW14_02, `1` = 1, `2` = 0),
    BW14_03 = recode(BW14_03, `1` = 0, `2` = 1),
    BW14_04 = recode(BW14_04, `1` = 1, `2` = 0),
    
    BW15_01 = recode(BW15_01, `1` = 1, `2` = 0),
    BW15_02 = recode(BW15_02, `1` = 0, `2` = 1),
    BW15_03 = recode(BW15_03, `1` = 0, `2` = 1),
    BW15_04 = recode(BW15_04, `1` = 1, `2` = 0)
  )

# 1.3 Bildung der BilWiss-Skalen ---------------------------------------------

# MC-Aufgaben: 0–2 Punkte pro Aufgabe (0/1/2 korrekt gewählte Alternativen)
# SC-Aufgaben: 0 oder 2 Punkte
bilwiss_data <- bilwiss_data %>%
  rowwise() %>%
  mutate(
    Punkte_Lernen_Entwicklung = sum(
      case_when(sum(c(BW01_01, BW01_02, BW01_03, BW01_04)) %in% c(0, 1) ~ 0,
                sum(c(BW01_01, BW01_02, BW01_03, BW01_04)) == 4 ~ 2,
                TRUE ~ 1),
      case_when(sum(c(BW02_01, BW02_02, BW02_03, BW02_04)) %in% c(0, 1) ~ 0,
                sum(c(BW02_01, BW02_02, BW02_03, BW02_04)) == 4 ~ 2,
                TRUE ~ 1),
      case_when(sum(c(BW04_01, BW04_02, BW04_03, BW04_04)) %in% c(0, 1) ~ 0,
                sum(c(BW04_01, BW04_02, BW04_03, BW04_04)) == 4 ~ 2,
                TRUE ~ 1),
      case_when(sum(c(BW06_01, BW06_02, BW06_03, BW06_04)) %in% c(0, 1) ~ 0,
                sum(c(BW06_01, BW06_02, BW06_03, BW06_04)) == 4 ~ 2,
                TRUE ~ 1),
      Punkte_BW03,
      Punkte_BW05,
      Punkte_BW10
    ),
    
    Punkte_Diagnostik = sum(
      case_when(sum(c(BW11_01, BW11_02, BW11_03, BW11_04)) %in% c(0, 1) ~ 0,
                sum(c(BW11_01, BW11_02, BW11_03, BW11_04)) == 4 ~ 2,
                TRUE ~ 1),
      case_when(sum(c(BW12_01, BW12_02, BW12_03, BW12_04)) %in% c(0, 1) ~ 0,
                sum(c(BW12_01, BW12_02, BW12_03, BW12_04)) == 4 ~ 2,
                TRUE ~ 1),
      case_when(sum(c(BW14_01, BW14_02, BW14_03, BW14_04)) %in% c(0, 1) ~ 0,
                sum(c(BW14_01, BW14_02, BW14_03, BW14_04)) == 4 ~ 2,
                TRUE ~ 1),
      case_when(sum(c(BW15_01, BW15_02, BW15_03, BW15_04)) %in% c(0, 1) ~ 0,
                sum(c(BW15_01, BW15_02, BW15_03, BW15_04)) == 4 ~ 2,
                TRUE ~ 1),
      Punkte_BW13,
      Punkte_BW16,
      Punkte_BW17,
      Punkte_BW22
    )
  ) %>%
  ungroup() %>%
  mutate(
    Gesamtpunkte = Punkte_Lernen_Entwicklung + Punkte_Diagnostik
  )

# Optionale Übersichtsausgabe
bilwiss_data %>%
  select(overall_id, Punkte_Lernen_Entwicklung, Punkte_Diagnostik, Gesamtpunkte) %>%
  print()

###############################################################################
# 2. Korrelationen: BilWiss-Skalen und FREE-Skalen (Ab/Rel/Pos)
###############################################################################

bilwiss_data_complete <- bilwiss_data %>%
  left_join(
    all_corr_with_scalemeans %>%
      select(
        overall_id,
        mean_abs, mean_rel, mean_pos,
        starts_with("AB"), starts_with("Rel"), starts_with("Pos")
      ),
    by = "overall_id"
  ) %>%
  filter(!is.na(mean_pos)) %>%   # nur Fälle mit vollständigen FREE-Skalen
  mutate(
    across(
      c(starts_with("AB"), starts_with("Rel"), starts_with("Pos")),
      ~ as.numeric(as.character(.))
    )
  )

# APA-formatierte Korrelationsmatrix (BilWiss-Skalen × FREE-Skalen)
apa.cor.table(
  bilwiss_data_complete %>%
    select(Punkte_Lernen_Entwicklung,
           Punkte_Diagnostik,
           Gesamtpunkte,
           mean_abs, mean_rel, mean_pos),
  filename           = "bilwiss_cor_free_scales_apa.doc",
  table.number       = 1,
  show.conf.interval = TRUE,
  show.sig.stars     = TRUE,
  landscape          = FALSE
)

###############################################################################
# 3. Korrelationen: BilWiss-Skalen und MERK-Items (Itemebene)
###############################################################################

# MERK-Items (AB01–AB13, Rel01–Rel13, Pos01–Pos13)
merk_items <- c(
  paste0("AB",  sprintf("%02d", 1:13)),
  paste0("Rel", sprintf("%02d", 1:13)),
  paste0("Pos", sprintf("%02d", 1:13))
)

# BilWiss-Skalen
bilwiss_skalen <- c(
  "Punkte_Lernen_Entwicklung",
  "Punkte_Diagnostik",
  "Gesamtpunkte"
)

# Korrelationsmatrix initialisieren
korrelationsmatrix_merk_vs_bilwiss <- matrix(
  NA,
  nrow = length(merk_items),
  ncol = length(bilwiss_skalen),
  dimnames = list(merk_items, bilwiss_skalen)
)

# paarweise Korrelationen berechnen (Itemebene × BilWiss-Skalen)
for (item in merk_items) {
  for (bw in bilwiss_skalen) {
    korrelationsmatrix_merk_vs_bilwiss[item, bw] <- cor(
      bilwiss_data_complete[[item]],
      bilwiss_data_complete[[bw]],
      use = "pairwise.complete.obs"
    )
  }
}

# Export der Korrelationsmatrix als Excel-Datei
openxlsx::write.xlsx(
  as.data.frame(korrelationsmatrix_merk_vs_bilwiss),
  file     = "Korrelation_MERK_Items_vs_BilWiss_Skalen.xlsx",
  rowNames = TRUE
)
###############################################################################
# Ende des Skripts zu BilWiss
###############################################################################
