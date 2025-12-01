
###############################################################################
# Skript: Korrelationsanalysen FREE × EBI-AM, LIST-K und FEE
#
# Ziel:
#  (1) Berechnung und Export von Korrelationsmatrizen zwischen den FREE-Skalen
#   (Absolutismus, Relativismus, Postrelativismus) und den externen Skalen
#    EBI-AM, LIST-K sowie FEE.
#  (2) Berechnung der Korrelationen auf Item-Ebene
#
# Voraussetzungen:
#   - Datenaufbereitungsskript ausgeführt
#
# Datengrundlage: 
#   - all.corr_with_scalemeans.RData (FREE, EBI, LIST, BilWiss inkl. Skalenmittelwerte)
#   - fee_free_means.RData (FREE, FEE, inkl. Skalenmittelwerte)
###############################################################################

library(dplyr)
library(apaTables)

###############################################################################
# 1. FREE × EBI-AM × LIST-K (Gesamtdatensatz all.corr)
###############################################################################

# Analyse-Datensatz laden
load("all.corr_with_scalemeans.RData")  # lädt Objekt: all.corr

# Relevante Skalenmittelwerte auswählen:
#   FREE:   mean_abs, mean_rel, mean_pos
#   EBI-AM: mean_mult, mean_abs_ebi
#   LIST-K: mean_elab, mean_crit, mean_rep, mean_reg
corr_scales_ebi_list <- all.corr %>%
  select(
    mean_abs, mean_rel, mean_pos,      # FREE
    mean_mult, mean_abs_ebi,           # EBI-AM
    mean_elab, mean_crit, mean_rep,    # LIST-K (kognitiv)
    mean_reg                           # LIST-K (metakognitiv)
  )

# Korrelationsmatrix im APA-Format exportieren
# Hinweis: Dateipfad vor Ausführung anpassen.
apa.cor.table(
  corr_scales_ebi_list,
  filename          = "Pfad_zum_Output/Korrelation_FREE_EBI_LIST.doc",
  table.number      = 1,
  show.conf.interval = TRUE,
  show.sig.stars     = TRUE,
  landscape          = FALSE
)


###############################################################################
# 2. FREE × FEE (Teilstichprobe fee_free_means)
###############################################################################

# Datensatz mit FREE- und FEE-Mittelwerten laden
load("fee_free_means.RData")  # lädt Objekt: fee_free_means

# Relevante Mittelwerte auswählen:
#   FREE: mean_abs, mean_rel, mean_pos
#   FEE:  mean_abs_fee, mean_rel_fee, mean_pos_fee
corr_scales_fee <- fee_free_means %>%
  select(
    mean_abs, mean_rel, mean_pos,           # FREE
    mean_abs_fee, mean_rel_fee, mean_pos_fee  # FEE
  )

# Korrelationsmatrix im APA-Format exportieren
# Hinweis: Dateipfad vor Ausführung anpassen.
apa.cor.table(
  corr_scales_fee,
  filename           = "Pfad_zum_Output/Korrelation_FREE_FEE.doc",
  table.number       = 2,
  show.conf.interval = TRUE,
  show.sig.stars     = TRUE,
  landscape          = FALSE
)

###############################################################################
# 3. Korrelationen auf Itemebene
#    - FREE-Items (Ab, Rel, Pos) × LIST-K / EBI-AM (all.corr)
#    - FREE-Items × FEE-Skalen (fee_free_means)
###############################################################################

library(openxlsx)

##### LIST-K & EBI-AM #########################################################

# Schritt 1: FREE-Items (Ab01–Ab13, Rel01–Rel13, Pos01–Pos13)
merk_items <- c(
  paste0("Ab",  sprintf("%02d", 1:13)),
  paste0("Rel", sprintf("%02d", 1:13)),
  paste0("Pos", sprintf("%02d", 1:13))
)

# Schritt 2: Validierungsvariablen (Skalenmittelwerte)
validierungsvariablen <- c(
  "mean_elab", "mean_crit", "mean_rep", "mean_reg",  # LIST-K
  "mean_abs_ebi", "mean_mult"                        # EBI-AM
)

# Schritt 3: Leere Matrix initialisieren
korrelationsmatrix <- matrix(
  NA,
  nrow = length(merk_items),
  ncol = length(validierungsvariablen),
  dimnames = list(merk_items, validierungsvariablen)
)

# Schritt 4: Korrelationen (paarweise vollständige Beobachtungen)
for (item in merk_items) {
  for (valid in validierungsvariablen) {
    korrelationsmatrix[item, valid] <- cor(
      all.corr[[item]],
      all.corr[[valid]],
      use = "pairwise.complete.obs"
    )
  }
}

# Export als Excel-Datei
write.xlsx(
  as.data.frame(korrelationsmatrix),
  file     = "Dateipfad/Korrelation_FREE_Items_vs_LIST_EBI.xlsx",
  rowNames = TRUE
)


##### FEE: FREE-Items × FEE-Skalen ############################################

# Schritt 1: FREE-Items (Ab01–Ab13, Rel01–Rel13, Pos01–Pos13)
merk_items <- c(
  paste0("Ab",  sprintf("%02d", 1:13)),
  paste0("Rel", sprintf("%02d", 1:13)),
  paste0("Pos", sprintf("%02d", 1:13))
)

# Schritt 2: Validierungsvariablen (Skalenmittelwerte aus fee_free_means)
fee_skalen <- c("mean_abs_fee", "mean_rel_fee", "mean_pos_fee")

# Schritt 3: Leere Matrix initialisieren
korrelationsmatrix_merk_vs_fee <- matrix(
  NA,
  nrow = length(merk_items),
  ncol = length(fee_skalen),
  dimnames = list(merk_items, fee_skalen)
)

# Schritt 4: Korrelationen (paarweise vollständige Beobachtungen) 
for (item in merk_items) {
  for (fee in fee_skalen) {
    korrelationsmatrix_merk_vs_fee[item, fee] <- cor(
      fee_free_means[[item]],
      fee_free_means[[fee]],
      use = "pairwise.complete.obs"
    )
  }
}

# Export als Excel-Datei
write.xlsx(
  as.data.frame(korrelationsmatrix_merk_vs_fee),
  file     = "Dateipfad/Korrelation_FREE_Items_vs_FEE_Skalen.xlsx",
  rowNames = TRUE
)


###############################################################################
# Ende des Korrelationsskripts
###############################################################################
