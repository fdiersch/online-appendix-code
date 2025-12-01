###############################################################################
# Skript: Reliabilitätsschätzungen externer Skalen
#
# Inhalt
# - Berechnug Mc Donalds Omega von:
#   (1) EBI-AM: Multiplismus, Absolutismus 
#   (2) LIST-K: Elaboration, Kritisches Prüfen, Wiederholen, Regulieren
#   (3) FEE: drei Subskalen 
#   (4) BilWiss: Skalen Lernen & Entwicklung, Diagnostik
# 
# Voraussetzungen:
#   - Datenaufbereitungsskript ausgeführt
#
# Datengrundlage: 
#   - all.corr_with_scalemeans.RData (FREE, EBI, LIST, BilWiss inkl. Skalenmittelwerte)
#   - fee_free_means.RData (FREE, FEE, inkl. Skalenmittelwerte)
###############################################################################

# Analyse-Datensatz laden
load("all.corr_with_scalemeans.RData")  # lädt Objekt: all.corr
load("fee_free_means.RData")  # lädt Objekt: fee_free:means

# Pakete laden ----------------------------------------------------------------

library(psych)
library(dplyr)

# Hilfsfunktion: Fälle entfernen, die auf allen Items einer Skala fehlend sind
clean_items <- function(df) {
  df[rowSums(!is.na(df)) > 0, ]
}

###############################################################################
# 1. EBI-AM: Multiplismus & Absolutismus
###############################################################################

# Itemlisten EBI-AM (Skalen nach Codebook)
items_ebi_mult <- c("EB01_01", "EB01_02", "EB01_03", "EB01_04", "EB01_05",
                    "EB01_06", "EB01_07", "EB01_08", "EB01_09", "EB01_10",
                    "EB01_11")

items_ebi_abs  <- c("EB01_12", "EB01_13", "EB01_14", "EB01_15",
                    "EB01_16", "EB01_17", "EB01_18", "EB01_19",
                    "EB01_20", "EB01_21", "EB01_22", "EB01_23")

# Datensätze pro Skala (nur Items, nur Fälle mit mindestens einem gültigen Item)
ebi_mult_dat <- all_corr[, items_ebi_mult] |> clean_items()
ebi_abs_dat  <- all_corr[, items_ebi_abs]  |> clean_items()

# McDonald’s omega für EBI-Absolutismus
omega_ebi_abs <- psych::omega(ebi_abs_dat,
                              nfactors = 1,
                              title    = "EBI Absolutismus",
                              plot     = FALSE)
print(omega_ebi_abs)
omega_ebi_abs$omega


# McDonald’s omega für EBI-Multiplismus
omega_ebi_mult <- psych::omega(ebi_mult_dat,
                               nfactors = 1,
                               title    = "EBI Multiplismus",
                               plot     = FALSE)
print(omega_ebi_mult)
omega_ebi_mult$omega   

##############################################################################
# 2. FEE: Sicherheit von Wissen (Ab), Umgang mit Autoritäten (Rel),
#         Reflexive Natur von Wissen (Pos)
##############################################################################
# Hinweis zum Datensatz fee_free:
# Die FEE-Skalen wurden im Rahmen einer separaten Teilstichprobe erhoben und liegen
# daher in einem eigenen Datensatz (fee_free) vor.
# Die Auswertung ist analog zur Vorgehensweise bei den übrigen externen Skalen.

# Itemlisten FEE (nach Subskalen)
items_fee_abs <- c("FE01_AB", "FE02_AB", "FE03_AB",
                   "FE04_AB", "FE05_AB", "FE06_AB", "FE07_AB")

items_fee_rel <- c("FE08_RE", "FE09_RE", "FE10_RE",
                   "FE11_RE", "FE12_RE", "FE13_RE")

items_fee_pos <- c("FE14_PO", "FE15_PO", "FE16_PO",
                   "FE17_PO", "FE18_PO")

# Datensätze pro Subskala (fee_free enthält FEE-Daten)
fee_abs_dat <- fee_free[, items_fee_abs] |> clean_items()
fee_rel_dat <- fee_free[, items_fee_rel] |> clean_items()
fee_pos_dat <- fee_free[, items_fee_pos] |> clean_items()

# McDonald’s omega je FEE-Subskala

# FEE: Sicherheit von Wissen
omega_fee_abs <- psych::omega(fee_abs_dat,
                              nfactors = 1,
                              title    = "FEE: Sicherheit von Wissen",
                              plot     = FALSE)
print(omega_fee_abs)
omega_fee_abs$omega

# FEE: Umgang mit Autoritäten
omega_fee_rel <- psych::omega(fee_rel_dat,
                              nfactors = 1,
                              title    = "FEE: Umgang mit Autoritäten",
                              plot     = FALSE)
print(omega_fee_rel)
omega_fee_rel$omega

# FEE: Reflexive Natur von Wissen
omega_fee_pos <- psych::omega(fee_pos_dat,
                              nfactors = 1,
                              title    = "FEE: Reflexive Natur von Wissen",
                              plot     = FALSE)
print(omega_fee_pos)
omega_fee_pos$omega

###############################################################################
# 3. LIST-K: Elaboration, Kritisches Prüfen, Wiederholen, Regulieren
###############################################################################

### Itemlisten LIST-K (nach Subskalen)
## Kognitive Lernstrategien
# Elaborieren
items_list_elab <- c("LI01_01", "LI01_02", "LI01_03")
# Kritisches Prüfen
items_list_crit <- c("LI01_04", "LI01_05", "LI01_06")
# Wiederholen
items_list_rep  <- c("LI01_07", "LI01_08", "LI01_09")
## Metakogntive Lernstrategie
# Selbstregulation
items_list_reg  <- c("LI01_10", "LI01_11", "LI01_12")

# Datensätze pro Subskala (nur Fälle mit mindestens einem gültigen Item)
list_elab_dat <- all_corr[, items_list_elab] |> clean_items()
list_crit_dat <- all_corr[, items_list_crit] |> clean_items()
list_rep_dat  <- all_corr[, items_list_rep]  |> clean_items()
list_reg_dat  <- all_corr[, items_list_reg]  |> clean_items()

# omega für LIST-K-Subskalen
omega_list_elab <- psych::omega(list_elab_dat,
                                nfactors = 1,
                                title    = "LIST Elaboration",
                                plot     = FALSE)
print(omega_list_elab)
omega_list_elab$omega

omega_list_crit <- psych::omega(list_crit_dat,
                                nfactors = 1,
                                title    = "LIST Kritisches Prüfen",
                                plot     = FALSE)
print(omega_list_crit)
omega_list_crit$omega

omega_list_rep <- psych::omega(list_rep_dat,
                               nfactors = 1,
                               title    = "LIST Wiederholen",
                               plot     = FALSE)
print(omega_list_rep)
omega_list_rep$omega

omega_list_reg <- psych::omega(list_reg_dat,
                               nfactors = 1,
                               title    = "LIST Regulieren",
                               plot     = FALSE)
print(omega_list_reg)
omega_list_reg$omega

###############################################################################
# 4. BilWiss: Skalen Lernen & Entwicklung, Diagnostik
###############################################################################

# Itemlisten BilWiss 
#BilWiss Lernen und Entwicklung
items_bw_le <- c(
  "BW01_01","BW01_02","BW01_03","BW01_04",
  "BW02_01","BW02_02","BW02_03","BW02_04",
  "BW04_01","BW04_02","BW04_03","BW04_04",
  "BW06_01","BW06_02","BW06_03","BW06_04",
  "BW03","BW05","BW10"
)
#Skala Diagnostik
items_bw_diag <- c(
  "BW11_01","BW11_02","BW11_03","BW11_04",
  "BW12_01","BW12_02","BW12_03","BW12_04",
  "BW14_01","BW14_02","BW14_03","BW14_04",
  "BW15_01","BW15_02","BW15_03","BW15_04",
  "BW13","BW16","BW17","BW22"
)


# BilWiss-Variablen bestimmen
bilwiss_vars <- all_corr %>%
  names() %>%
  grep("^BW", ., value = TRUE)

bilwiss_data <- all_corr %>%
  dplyr::select(overall_id, all_of(bilwiss_vars))

# Datensätze pro Subskala (nur Fälle mit mindestens einem gültigen Item)
bw_le_dat   <- bilwiss_data[, items_bw_le]   |> clean_items()
bw_diag_dat <- bilwiss_data[, items_bw_diag] |> clean_items()

# omega für BilWiss-Skalen
#BilWiss Lernen und Entwicklung
omega_bw_le <- psych::omega(bw_le_dat,
                            nfactors = 1,
                            plot     = FALSE)
print(omega_bw_le)
omega_bw_le$omega

#BilWiss Diagnostik
omega_bw_diag <- psych::omega(bw_diag_dat,
                              nfactors = 1,
                              plot     = FALSE)
print(omega_bw_diag)
omega_bw_diag$omega
###############################################################################
# Ende des Skripts für Reliabilitätsschätzungen externer Skalen
###############################################################################