# -------------------------------------------------------------------------#
# 0 HGF depletion Model ----
# -------------------------------------------------------------------------#
#
# MM-00.0.R
#
# [PURPOSE]
# 
# Reactions as proposed by Lorenza
#
#
# [AUTHOR]
# model by Svenja 
# data by Lorenza
#
# [Date]
# 2021-03-08
# 

rm(list = ls(all.names = TRUE))
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
.currentwd <- getwd()
.build <- TRUE
.test.SS <- FALSE
.doLog <- TRUE
.dataFolder       <- "../01-Data"
.resultsFolder     <- "../04-Results/MM-00.0"
.modelFolder      <- file.path(.resultsFolder, "01-Model")
.fitFolder        <- file.path(.resultsFolder, "02-Fits")
.profileFolder    <- file.path(.resultsFolder, "03-Profiles")
.plotFolder       <- file.path(.resultsFolder, "04-Plots")

for (x in c(.resultsFolder,.modelFolder,.fitFolder,.plotFolder,.profileFolder))
  if (!dir.exists(x)) dir.create(x, recursive = TRUE)

# .. Load Libraries -----
library(dMod)
library(deSolve)
library(rootSolve)
library(dplyr)
library(tidyverse)
library(dplyrExtras)
library(conveniencefunctions)
library(parallel)
source("../02-Scripts/Functions/UsefulFunctions.R")

# -------------------------------------------------------------------------#
# 1 Load data ----
# -------------------------------------------------------------------------#

HGFdata <- fread(file.path(.dataFolder, "01-HGFdepletion_Data.rds")) %>% .[, unit_HGFs := NULL] #%>% .[time < 400]
Metdata <- fread(file.path(.dataFolder, "01-Met_Data.rds")) 

mdata <- rbind(HGFdata, Metdata)
if(.doLog) {
  mdata[, ":=" (sigma = sigma/(value*log(10)))]
  mdata[, ":=" (value = log10(value))] 
}

mydata <- mdata %>% 
  as.data.frame() %>% 
  as.datalist(split.by = c("HGF")) 

# -------------------------------------------------------------------------#
# 2 Model definition ----
# -------------------------------------------------------------------------#

# .. 1 Observables -----
observables <- eqnvec(
  pMet_au = "scale_pMet * (pMet_HGF + pMet_HGF_int) + offset_pMet",
  tMet_au = "scale_tMet * (Met + Met_HGF + pMet_HGF + pMet_HGF_int + Met_int) + offset_tMet",
  HGF_au = "scale_HGF * HGF + offset_HGF"
  )

# log-transform observables
if(.doLog) observables <- as.eqnvec(paste("log10(", observables, ")"), names = names(observables))

# .. 2 Reactions -----
reactions <- NULL %>% 

  # SS
  addReaction("", "Met", rate = "k_prod", description = "Met production") %>% 
  addReaction("Met", "", rate = "k_deg*Met", description = "Met degradation") %>% 
  
  addReaction("Met + HGF", "Met_HGF", rate = "k_ass * Met * HGF", description = "HGF to MET association (no steady state)") %>% 
  addReaction("Met_HGF + Met_HGF", "pMet_HGF + pMet_HGF", rate = "k_act * Met_HGF * Met_HGF", description = "Met activation") %>% 
  
  addReaction("pMet_HGF", "pMet_HGF_int", rate = "k_int * pMet_HGF", description = "Met internalization") %>% 
  addReaction("pMet_HGF_int", "Met_int + HGF_int", rate = "k_diss * pMet_HGF_int", description = "pMet_HGF_int dissociation") %>%
  addReaction("Met_int", "Met", rate = "k_rec*Met_int", description = "Met recycling") %>% 
  addReaction("Met_int", "", rate = "k_degint*Met_int", description = "intracellular Met degradation") %>% 
  addReaction("HGF_int", "", rate = "k_degHGF*HGF_int", description = "HGF degradation") %>% 
  
  {.}

# .. 3 Define steady state -----
reactions_SS <- subset(reactions, !grepl("no steady state", Description))
# steady_states <- resolveRecurrence(steadyStates(reactions_SS))
steady_states <- c(
  Met_HGF = 0,
  pMet_HGF = 0,
  pMet_HGF_int = 0,
  Met_int = 0,
  HGF_int = 0,
  Met = "k_prod/k_deg"
)

# -------------------------------------------------------------------------#
# 3 Parameter transformation ----
# -------------------------------------------------------------------------#
# .. 1 condition.grid -----
cg <- attr(mydata, "condition.grid") 
condition.grid <- cg %>% 
  mutate(condition = rownames(cg)) %>%
  mutate(ID = 1:nrow(cg)) 

myconditions <- condition.grid$condition
rownames(condition.grid) <- myconditions

# .. 2 trafo original -----
innerpars <- unique(c(getParameters(reactions), getSymbols(observables)))
trafo <- define(NULL, "x~y", x = innerpars, y = innerpars) %>% 
  insert("x~y", x = names(steady_states), y = steady_states) %>% # SS trafo
  {.}

## SS-Test
if(.test.SS) trafo <- repar("x~0", trafo, x = c("HGF"))

# .. 3 trafo list original -----
trafoL <- branch(trafo, table = condition.grid) %>% 
  insert("x~y", 
         x = "HGF", y = HGF) %>% 
  
  insert("x ~ exp(x)", x = .currentSymbols) %>% 
{.}



# -------------------------------------------------------------------------#
# 4 Build dMod objects ----
# -------------------------------------------------------------------------#

if (.build){
  setwd(.modelFolder)
  
  # .. 1.1 Generate ODEmodel ----- 
  x <- odemodel(reactions, 
                modelname = "x", 
                compile = FALSE) %>% Xs(
                  optionsSens = list(method = "lsodes", rtol = 1e-8, atol = 1e-8))
  #condition = NULL, 
  
  # .. 2 Generate Observation function -----
  g <- Y(observables, f = reactions, condition = NULL,
         compile = F, modelname = "g")

  # .. 4 Define parameter trafo -----
  p <- P(trafoL, modelname = "p")

  # .. 5 Compile and Export -----
  soname <- str_remove_all(paste0(basename(.resultsFolder)), c("-|_"))
  compile(x, g, p, output = soname, cores  = detectFreeCores())
  save(x, g, p, file = "001-model.rds")
  setwd(.currentwd)
  
} else {
  # .. 1.2 Load DLLs -----
  setwd(.modelFolder)
  load("001-model.rds")
  loadDLL(x)
  setwd(.currentwd)
}

# .. 2 pouter -----
outerpars <- getParameters(p)
pouter <- structure(rep(-1, length(outerpars)), names = outerpars)
# pouter <- structure(runif(length(outerpars), -1, 1), names = outerpars)
ini <- pouter

# -------------------------------------------------------------------------#
# 5 Test prediction and obj ----
# -------------------------------------------------------------------------#

prd0 <- (g*x*p)
times <- seq(0, max(mdata$time), 1)

prd <- prd0(times, ini)
obj <- normL2(data = mydata, x = prd0, times = times) + 
  constraintL2(ini, sigma = 12) 
# obj(ini)


if(FALSE){
  common <- intersect(names(ini),names(bestfit))
  ini[common] <- bestfit[common]
  
  common <- intersect(names(ini),names(myfit$argument))
  ini[common] <- myfit$argument[common]
}

# plotCombined(prd0(times, pouter), mydata)

# Exit ----
