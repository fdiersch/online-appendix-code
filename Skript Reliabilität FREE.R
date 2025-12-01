###############################################################################
# Skript: Berechnung Reliabilitäten FREE (Absolutismus, Relativismus, Postrel.)
#
# Ziel:
#  (1) Berechnung von McDonald's Omega für die drei FREE-Subskalen
#      auf Basis des zusammengeführten FREE-Datensatzes (free_subs_all).
#
# Voraussetzungen:
#   - Datenaufbereitungsskript ausgeführt
#
# Datengrundlage:
#   - free_subs_all: Datensatz mit allen FREE-Items 
#   (bereits auf Fälle mit mindestens einem gültigen FREE-Item reduziert).
###############################################################################

# Pakete laden 
library(psych)
library(dplyr)

# Datensatz laden
load("free_subs_all.RData")  # lädt Objekt: free_subs_all

# Itemlisten pro Subskala (Items 1–13)
items_ab <- c("Ab01","Ab02","Ab03","Ab04","Ab05","Ab06","Ab07","Ab08", "Ab09",
              "Ab10","Ab11","Ab12","Ab13")

items_re <- c("Rel01","Rel02","Rel03","Rel04","Rel05",
              "Rel06","Rel07","Rel08","Rel09","Rel10","Rel11","Rel12","Rel13")

items_po <- c("Pos01","Pos02","Pos03","Pos04","Pos05","Pos06","Pos07","Pos08",
              "Pos09","Pos10","Pos11","Pos12","Pos13")

# Omega-Berechnung pro Skala
# Hinweis: plot = FALSE unterdrückt den grafischen Output.
# Omega Absolutismus
omega_ab <- psych::omega(free_subs_all[items_ab],
                         title = "FREE – Absolutismus",
                         plot  = FALSE)
print(omega_ab)
# Omega Relativismus
omega_re <- psych::omega(free_subs_all[items_re],
                         title = "FREE – Relativismus",
                         plot  = FALSE)
print(omega_re)
# Omega Postrelativismus
omega_po <- psych::omega(free_subs_all[items_po],
                         title = "FREE – Postrelativismus",
                         plot  = FALSE)
print(omega_po)


###############################################################################
# Ende des Skripts zur Berechnung der Reliabilitäten der FREE-Skalen
###############################################################################
