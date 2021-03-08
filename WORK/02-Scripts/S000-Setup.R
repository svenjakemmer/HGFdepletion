# -------------------------------------------------------------------------#
# 0 Setup Model ----
# -------------------------------------------------------------------------#
#
# SetupModel.R
#
# [PURPOSE]
# 
# Script for the model evaluation
#
#
# [AUTHOR]
# Svenja Kemmer
#
# [Date]
# 2021-03-08 
#
# -------------------------------------------------------------------------#
# 1 Optimization ----
# -------------------------------------------------------------------------#

# .. 1 Fits -----
run <- "1"
fit.name <- "M_1trust"
  
SendFits(recover = T, mypartition = "multi")
SendFitsKnechte(recover = T, myknecht = "knecht1")
GetFits()
# load myframe 
myframe <- readRDS(file = file.path(.fitFolder, paste0("myframe_1.run.rds")))

# plot and save
PlotWaterfall()
PlotParValues(fits = c(1:10))
PlotObsParValues(fits = c(1:10))

# .. 2 Define Bestfit -----
step <- 1
bestfit <- as.parvec(myframe, step)
fitvalue <- round(myframe[step]$value, digits = 2)
SaveBestfit()

# .. 3 Plots -----
PlotObservables()
PlotObsDR()
PlotStates()
PlotStatesDR()

# .. 4 Purge fits -----
eval(parse(text=paste0(fit.name, "$purge()"))) 


# -------------------------------------------------------------------------#
# 2 Identifiability ----
# -------------------------------------------------------------------------#

# .. 1 Profiles -----
profile.name <- "job_prof1files"

SendProfiles(recover = F, nodetype = "bestplus", method = "integrate")
# get and save, needs fitvalue
GetProfiles()

profiles <- readRDS(file = file.path(.profileFolder, paste0("profiles_", run, "_step", step, "_", fitvalue, ".rds")))

# plot and save plot
PlotProfiles()

grep("drug_d_", outerpars, value = T)
plotProfile(subset(profiles, whichPar %in% grep("drug_d_", outerpars, value = T)), mode == "data")
plotPaths(profiles, whichPar = "k_phospho_AKT")

PurgeProfiles()

PlotArray(myPar = "Kd_pcRAF_R1R1", 
          direction = "up", 
          name %in% grep("R1",reactions$states, value = T) & ligand == "BTC" & sample == "MCF7")#  


# for(i in profiles$whichPar %>% unique()){
#   print(i)
#   test <- subset(profiles, whichPar == i)
#   test$constraint %>% as.numeric()
# }
# 
# for(i in names(myprofile)[!names(myprofile) %in% "whichPar"]){
#   myprofile[[i]] <- as.numeric(myprofile[[i]])
# }


# -------------------------------------------------------------------------#
# 3 L1 analysis ----
# -------------------------------------------------------------------------#

fit.name <- "job_ms2trust"

mylambdas <- c(-2, -1, 0)
SendL1Fits(recover = T)
GetL1Fits()
PlotL1Fits(orderby = -1)

