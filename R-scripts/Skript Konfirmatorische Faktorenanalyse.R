###############################################################################
# Skript: Konfirmatorische Faktorenanalysen (CFA) für den FREE
# 
# Ziel:
#   (1) Einfaktorielle CFAs für die drei Skalen (Voranalyse)
#   (2) Dreifaktorenmodell (Modell CFA 1)
#   (3) Dreifaktorenmodell mit Residualkorrelationen (Modell CFA 2a)
#   (4) Dreifaktorenmodell mit Methodenfaktoren (Modell CFA 2b)
#
# Voraussetzungen:
#   - Datenaufbereitungsskript ausgeführt
#
# Datengrundlage:
#   - free_subs_all: Datensatz mit allen FREE-Items 
#   (bereits auf Fälle mit mindestens einem gültigen FREE-Item reduziert).
###############################################################################

# Hinweis: Die folgenden Pakete müssen vorab installiert sein.
###############################################################################

# Laden der benötigten Pakete -------------------------------------------------

library(tidyverse)       
library(lavaan)         
library(psych)           
library(MBESS)           
library(misty)           
library(mice)            
library(data.table)      
library(readxl)          
library(GPArotation)    
library(semPlot)         
library(coefficientalpha) 

# Datensatz laden
load("free_subs_all.RData") # lädt Objekt: free_subs_all


##### FAKTORENANALYSE #########################################################

### Modellzuordnung
Absolutismus =~ Ab01 + Ab02 + Ab03 + Ab04 + Ab05 + Ab06 + Ab07 + Ab08 + Ab09 + Ab10 + Ab11 + Ab12 + Ab13
Relativismus =~ Rel01 + Rel02 + Rel03 + Rel04 + Rel05 + Rel06 + Rel07 + Rel08 + Rel09 + Rel10 + Rel11 + Rel12 + Rel13
Postrelativismus =~ Pos01 + Pos02 + Pos03 + Pos04 + Pos05 + Pos06 + Pos07 + Pos08 + Pos09 + Pos10 + Pos11 + Pos12 + Pos13


#### Voranalyse: Einfaktorielle CFAs pro Skala ################################

# Absolutismus Modelldefinition
modell_Ab <- '
Absolutismus =~ Ab01 + Ab02 + Ab03 + Ab04 + Ab05 + Ab06 + Ab07 + Ab08 + Ab09 + Ab10 + Ab11 + Ab12 + Ab13
'
# Modell analysieren
Testergebnis_Ab <- cfa(model = modell_Ab,
                       data = free_subs_all,
                       estimator = "mlr",
                       missing = "fiml")

# Ergebnisse anschauen
summary(Testergebnis_Ab, fit.measures = TRUE, standardized = TRUE)

# Relativismus Modelldefinition
modell_RE <- '
Relativismus =~ Rel01 + Rel02 + Rel03 + Rel04 + Rel05 + Rel06 + Rel07 + Rel08 + Rel09 + Rel10 + Rel11 + Rel12 + Rel13
'
# Modell analysieren
Testergebnis_RE <- cfa(model = modell_RE,
                       data = free_subs_all,
                       estimator = "mlr",
                       missing = "fiml")

# Ergebnisse anschauen
summary(Testergebnis_RE, fit.measures = TRUE, standardized = TRUE)
semPaths(Testergebnis_RE, whatLAbels = "stand")


# Postrelativismus Modelldefinition
modell_PO <- '
Postrelativismus =~ Pos01 + Pos02 + Pos03 + Pos04 + Pos05 + Pos06 + Pos07 + Pos08 + Pos09 + Pos10 + Pos11 + Pos12 + Pos13
'
# Modell analysieren
Testergebnis_PO <- cfa(model = modell_PO,
                       data = free_subs_all,
                       estimator = "mlr",
                       missing = "fiml")

# Ergebnisse anschauen
summary(Testergebnis_PO, fit.measures = TRUE, standardized = TRUE)

#### Hauptanalysen ######################################

### Modell 1: CFA 
# Modelldefinition
modell_GE1 <- '
Absolutismus =~ Ab01 + Ab02 + Ab03 + Ab04 + Ab05 + Ab06 + Ab07 + Ab08 + Ab09 + Ab10 + Ab11 + Ab12 + Ab13
Relativismus =~ Rel01 + Rel02 + Rel03 + Rel04 + Rel05 + Rel06 + Rel07 + Rel08 + Rel09 + Rel10 + Rel11 + Rel12 + Rel13
Postrelativismus =~ Pos01 + Pos02 + Pos03 + Pos04 + Pos05 + Pos06 + Pos07 + Pos08 + Pos09 + Pos10 + Pos11 + Pos12 + Pos13
'
# Modell analysieren
Testergebnis_GE1 <- cfa(model = modell_GE1,
                        data = free_subs_all,
                        estimator = "mlr",
                        missing = "fiml")

# Ergebnisse anschauen
summary(Testergebnis_GE1, fit.measures = TRUE, standardized = TRUE)

### Modell 2a: CFA mit Residualkorrelationen zwischen Items desselben Itemstamms ###
# Modelldefinition
modell_GE2a <- '
Absolutismus =~ Ab01 + Ab02 + Ab03 + Ab04 + Ab05 + Ab06 + Ab07 + Ab08 + Ab09 + Ab10 + Ab11 + Ab12 + Ab13
Relativismus =~ Rel01 + Rel02 + Rel03 + Rel04 + Rel05 + Rel06 + Rel07 + Rel08 + Rel09 + Rel10 + Rel11 + Rel12 + Rel13
Postrelativismus =~ Pos01 + Pos02 + Pos03 + Pos04 + Pos05 + Pos06 + Pos07 + Pos08 + Pos09 + Pos10 + Pos11 + Pos12 + Pos13

# Residualkorrelationen zwischen Items desselben Itemstamms
Rel01~~Ab01
Pos01~~Ab01
Rel01~~Pos01

Rel02~~Ab02
Pos02~~Ab02
Rel02~~Pos02

Rel03~~Ab03
Pos03~~Ab03
Rel03~~Pos03

Rel04~~Ab04
Pos04~~Ab04
Rel04~~Pos04

Rel05~~Ab05
Pos05~~Ab05
Rel05~~Pos05

Rel06~~Ab06
Pos06~~Ab06
Rel06~~Pos06

Rel07~~Ab07
Pos07~~Ab07
Rel07~~Pos07

Rel08~~Ab08
Pos08~~Ab08
Rel08~~Pos08

Rel09~~Ab09
Pos09~~Ab09
Rel09~~Pos09

Rel10~~Ab10
Pos10~~Ab10
Rel10~~Pos10

Rel11~~Ab11
Pos11~~Ab11
Rel11~~Pos11

Rel12~~Ab12
Pos12~~Ab12
Rel12~~Pos12

Rel13~~Ab13
Pos13~~Ab13
Rel13~~Pos13
'
# Modell analysieren
Testergebnis_GE2a <- cfa(model = modell_GE2a,
                         data = free_subs_all,
                         estimator = "mlr",
                         missing = "fiml")
# Ergebnisse anschauen
summary(Testergebnis_GE2a, fit.measures = TRUE, standardized = TRUE)


### Modell 2b: Dreifaktoren-CFA mit Methodenfaktoren für Items desselben Itemstamms ###

modell_GE2b <- '
Absolutismus =~ Ab01 + Ab02 + Ab03 + Ab04 + Ab05 + Ab06 + Ab07 + Ab08 + Ab09 + Ab10 + Ab11 + Ab12 + Ab13
Relativismus =~ Rel01 + Rel02 + Rel03 + Rel04 + Rel05 + Rel06 + Rel07 + Rel08 + Rel09 + Rel10 + Rel11 + Rel12 + Rel13
Postrelativismus =~ Pos01 + Pos02 + Pos03 + Pos04 + Pos05 + Pos06 + Pos07 + Pos08 + Pos09 + Pos10 + Pos11 + Pos12 + Pos13

# Methodenfaktoren für Items desselben Itemstamms
#FREE01 =~ 1*Ab01 + 1*Rel01 + 1*Pos01 nicht spezifiziert für die Modellidentifikation
FREE02 =~ 1*Ab02 + 1*Rel02 + 1*Pos02
FREE03 =~ 1*Ab03 + 1*Rel03 + 1*Pos03
FREE04 =~ 1*Ab04 + 1*Rel04 + 1*Pos04
FREE05 =~ 1*Ab05 + 1*Rel05 + 1*Pos05
FREE06 =~ 1*Ab06 + 1*Rel06 + 1*Pos06
FREE07 =~ 1*Ab07 + 1*Rel07 + 1*Pos07
FREE08 =~ 1*Ab08 + 1*Rel08 + 1*Pos08
FREE09 =~ 1*Ab09 + 1*Rel09 + 1*Pos09
FREE10 =~ 1*Ab10 + 1*Rel10 + 1*Pos10
FREE11 =~ 1*Ab11 + 1*Rel11 + 1*Pos11
FREE12 =~ 1*Ab12 + 1*Rel12 + 1*Pos12
FREE13 =~ 1*Ab13 + 1*Rel13 + 1*Pos13

# Inhalts- und Methodenfaktoren sind unkorreliert
#Absolutismus~~0*FREE01 nicht spezifiziert für die Modellidentifikation
Absolutismus~~0*FREE02
Absolutismus~~0*FREE03
Absolutismus~~0*FREE04
Absolutismus~~0*FREE05
Absolutismus~~0*FREE06
Absolutismus~~0*FREE07
Absolutismus~~0*FREE08
Absolutismus~~0*FREE09
Absolutismus~~0*FREE10
Absolutismus~~0*FREE11
Absolutismus~~0*FREE12
Absolutismus~~0*FREE13
#Relativismus~~0*FREE01 nicht spezifiziert für die Modellidentifikation
Relativismus~~0*FREE02
Relativismus~~0*FREE03
Relativismus~~0*FREE04
Relativismus~~0*FREE05
Relativismus~~0*FREE06
Relativismus~~0*FREE07
Relativismus~~0*FREE08
Relativismus~~0*FREE09
Relativismus~~0*FREE10
Relativismus~~0*FREE11
Relativismus~~0*FREE12
Relativismus~~0*FREE13
#Postrelativismus~~0*FREE01 nicht spezifiziert für die Modellidentifikation
Postrelativismus~~0*FREE02
Postrelativismus~~0*FREE03
Postrelativismus~~0*FREE04
Postrelativismus~~0*FREE05
Postrelativismus~~0*FREE06
Postrelativismus~~0*FREE07
Postrelativismus~~0*FREE08
Postrelativismus~~0*FREE09
Postrelativismus~~0*FREE10
Postrelativismus~~0*FREE11
Postrelativismus~~0*FREE12
Postrelativismus~~0*FREE13'

# Modell analysieren
Testergebnis_GE2b <- cfa(model = modell_GE2b,
                         data = free_subs_all,
                         estimator = "mlr",
                         missing = "fiml")

# Ergebnisse anschauen
summary(Testergebnis_GE2b, fit.measures = TRUE, standardized = TRUE)

###############################################################################
# Ende des CFA Skripts
###############################################################################
