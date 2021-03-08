# -------------------------------------------------------------------------#
# 0 BlotIt Preprocessing for MET data ----
# -------------------------------------------------------------------------#
#
# Setup_BlotIt.R
#
# [PURPOSE]
# 
# Bring MET data to common scale
#
#
# [AUTHOR]
# Svenja Kemmer
#
# [Date]
# 2021-03-08 
#
rm(list = ls(all.names = TRUE))
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))

## Load packages ##
library("blotIt2")
library("stats")
library("utils")
library("grDevices")
library("graphics") # utils, grDevices and graphics should in theory be loaded by the stats package
library("grid")
library("gridExtra")
library("WriteXLS")
library("dplyr")
library("tidyr")
library(conveniencefunctions)

.currentwd <- getwd()
.dataOrigFolder <- "../00-DataOriginal"
.dataFolder <- "../01-Data"
.resultsFolder <- "../04-Results/S002-ScaleBlotIt"

for (x in c(.resultsFolder))
  if (!dir.exists(x)) dir.create(x, recursive = TRUE)

# -------------------------------------------------------------------------#
# 1 Read data ----
# -------------------------------------------------------------------------#
file <- "Total_Met_data_for_HGF-Met_degradation/Data_Time course_Dose response_pMet_tMet.csv"
myfile <- read.wide(file = file.path(.dataOrigFolder, file), sep = ",", dec=".", header=TRUE, description = 1:3) %>% as.data.table()
mydata <- myfile[HGF_input %in% c(40)]
setnames(mydata, "HGF_input", "HGF")
# -------------------------------------------------------------------------#
# 2 Scale BlotIt ----
# -------------------------------------------------------------------------#

# .. 1 Run alignME -----
out <- alignME(data=mydata, #[condition == "WD"&name == "tMet"&membrane%in%grep("B3", membrane, value = TRUE)]
               model = "ys/sj",
               errmodel = "sigmaR*value",
               fixed = ys~HGF, # (name, time are included automatically)
               latent = sj~Gel, #or membrane - to be used for scaling
               error = sigmaR~1,
               log = TRUE)
data <- out %>% as.data.table()

# .. 2 Save scald data -----
fwrite(data, file.path(.dataFolder, "01-Met_Data.rds"))

# -------------------------------------------------------------------------#
# 3 Data analysis plots ----
# -------------------------------------------------------------------------#

cairo_pdf(filename = file.path(.resultsFolder, paste0("SummaryPlots.Met.pdf")), onefile = TRUE, width = 10, height = 8) #pdf öffnen
  plotNew1(out)
  plotNew2(out)
  plotNew3(out)
dev.off() #pdf schließen


# -------------------------------------------------------------------------#
# 4 Plot HGF depletion data ----
# -------------------------------------------------------------------------#

# .. 0 Lead data -----
HGFdata <- read.wide(file = file.path(.dataOrigFolder, "HGF_Depletion/HGF_depletion_LA.csv"), sep = ",", dec=".", header=TRUE, description = c(1,2,4)) %>% as.data.table()
HGFdata[,Gel:=1]

# .. 1 Run alignME -----
out2 <- alignME(data=HGFdata, #[condition == "WD"&name == "tMet"&membrane%in%grep("B3", membrane, value = TRUE)]
               model = "ys/sj",
               errmodel = "sigmaR*value",
               fixed = ys~HGF, # (name, time are included automatically)
               latent = sj~Gel, #or membrane - to be used for scaling
               error = sigmaR~1,
               log = TRUE)

data2 <- out %>% as.data.table()

# .. 2 Save scald data -----
fwrite(data2, file.path(.dataFolder, "01-HGFdepletion_Data.rds"))

# .. 3 Plots -----
cairo_pdf(filename = file.path(.resultsFolder, paste0("SummaryPlots.HGFdepletion.pdf")), onefile = TRUE, width = 6, height = 6) #pdf öffnen
  plotNew1(out2)
  plotNew2(out2)
  plotNew3(out2)
dev.off() #pdf schließen

# Exit ----


