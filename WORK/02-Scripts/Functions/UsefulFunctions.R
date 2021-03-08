# -------------------------------------------------------------------------#
# 0 Define useful functions ----
# -------------------------------------------------------------------------#
#
# UsefulFunctions.R
#
# [PURPOSE]
# 
# Send fits or profiles to the cluster
#
# [AUTHOR]
# Svenja
#
# [Date]
# 2020-06-19
#

# -------------------------------------------------------------------------#
# 1 Optimization ----
# -------------------------------------------------------------------------#

#' Run fits on cluster
#'
#' @param jobname job name on cluster
#' @param recover 
#' @param nodetype 
#'
#' @return
#' @export
#'
#' @examples
SendFits <- function(jobname = fit.name, recover = T, mypartition = "single"){
  setwd(.currentwd)
  mynodes <- 10
  
  #send fits
  setwd(.modelFolder)
  assign(jobname, runbg_bwfor_slurm({
    mstrust(obj, ini, rinit = 1, rmax = 10, iterlim=40000, 
            sd = 3, parupper = 12, parlower = -12, 
            fits = 1*16, cores = 16, fixed = NULL)
  }, machine = "cluster", partition = mypartition, nodes = mynodes, cores = 16, walltime = "30:00:00", filename = jobname, 
  compile = FALSE, recover = recover, password = mypassword))
  setwd(.currentwd)
  
  # make function globally available
  assign(jobname, eval(parse(text = jobname)), envir=globalenv())
  
  # Message
  if(recover == F) print(paste0(mynodes*16," fits have been successfully uploaded to the cluster under the name ", jobname, "."))
  if(recover == T) print(paste0(mynodes*16," fits have been successfully recovered for ", jobname, "."))
}


#' Run fits on Knechte
#'
#' @param jobname job name 
#' @param recover 
#'
#' @return
#' @export
#'
#' @examples
SendFitsKnechte <- function(jobname = fit.name, recover = T, myknecht = "knecht1"){
  setwd(.currentwd)

  #send fits
  setwd(.modelFolder)
  assign(jobname, runbg({
    mstrust(obj, ini, rinit = 1, rmax = 10, iterlim=40000, 
            sd = 3, parupper = 12, parlower = -12, 
            cores = detectFreeCores(), fits = 100)
  }, machine = myknecht, filename = jobname, input = c("obj", "ini", "e"), compile = FALSE, recover = recover))
  setwd(.currentwd)
  
  # make function globally available
  assign(jobname, eval(parse(text = jobname)), envir=globalenv())
  
  # Message
  if(recover == F) print(paste0("Fits have been successfully uploaded to ", myknecht, " under the name ", jobname, "."))
  if(recover == T) print(paste0("Fits have been successfully recovered for ", jobname, "."))
}

#' Get cluster jobs
#'
#' @param runname 
#' @param jobname
#'
#' @return
#' @export
#'
#' @examples
GetFits <- function(runname = run, jobname = fit.name){
  setwd(.currentwd)
  setwd(file.path(.fitFolder))
  # check
  print(eval(parse(text = paste0(jobname,"$check()"))))
  #get
  eval(parse(text = paste0(jobname,"$get()")))
  # merge in myframe
  myframe <- do.call(rbind,lapply(.runbgOutput, function(knechtoutput){
    if(length(knechtoutput)>0)  
      if(is(knechtoutput,"try-error")) NULL 
    else as.parframe(knechtoutput)
    else NULL
  }))
  myframe <- myframe[order(myframe$value),]
  setwd(.currentwd)
  
  # save myframe
  run <- runname
  saveRDS(myframe, file = file.path(.fitFolder, paste0("myframe_", runname, ".run.rds")))
  
  # make myframe globally availible
  assign("myframe", myframe, envir=globalenv())
}


#' Save bestfit
#'
#' @param runname 
#' @param fitnumber
#'
#' @return
#' @export
#'
#' @examples
SaveBestfit <- function(fitnumber = step, runname = run){
  bestfit <- as.parvec(myframe, fitnumber)
  fitvalue <- round(myframe[fitnumber]$value, digits = 2)
  saveRDS(bestfit, file = file.path(.fitFolder, paste0("bestfit_", runname, "_step", fitnumber, "_", fitvalue, ".rds")))
  return(fitvalue)
}



# -------------------------------------------------------------------------#
# 2 Plotting ----
# -------------------------------------------------------------------------#

#' Waterfall plot
#'
#' @param runname 
#'
#' @return
#' @export
#'
#' @examples
PlotWaterfall <- function(runname = run){
  pl <- plotValues(myframe)
  ggsave(file.path(.plotFolder, paste0("001-Waterfall_", runname, ".run.pdf")), pl, width = 10, height = 8, device = cairo_pdf)
  print(pl)
  print(myframe$value[c(1:20)])
}

#' ParValue plot
#'
#' @param runname 
#'
#' @return
#' @export
#'
#' @examples
PlotParValues <- function(runname = run, steptol = 1, fits = 1:5){
  common_names <- subset(names(myframe), !grepl(paste(c("*sigma*","*scale*","*offset*"), collapse="|"),names(myframe)))
  pl <- plotPars(myframe[c(fits),common_names], tol = steptol)
  ggsave(file.path(.plotFolder, paste0("002-ParValues_", runname, "run.pdf")), pl, width = 30, height = 10, device = cairo_pdf)
  print(pl)
}

#' ObsParValue plot
#'
#' @param runname 
#'
#' @return
#' @export
#'
#' @examples
PlotObsParValues <- function(runname = run, steptol = 1, fits = 1:5){
  common_names <- subset(names(myframe), grepl(paste(c("index", "value", "converged", "iterations","*sigma*","*scale*","*offset*"), collapse="|"),names(myframe)))
  pl <- plotPars(myframe[c(fits),common_names], tol = steptol)
  ggsave(file.path(.plotFolder, paste0("002-ObsParValues_", runname, "run.pdf")), pl, width = 30, height = 10, device = cairo_pdf)
  print(pl)
}


#' ParValue plot of 2 frames
#'
#' @param runname 
#'
#' @return
#' @export
#'
#' @examples
PlotPars2gether <- function(model1 = "M07-ModelExtension2_log", model2 = "M07-ModelExtension2MDA231_log", fits = c(1,1), run = c("1", "1"),steptol = 0.001){
  # load frames
  frame1 <- readRDS(file = paste0("../04-Results/", model1, "/02-Fits/myframe_", run[1], ".run.rds"))
  frame2 <- readRDS(file = paste0("../04-Results/", model2, "/02-Fits/myframe_", run[2], ".run.rds"))
  # make compareframe
  common <- intersect(names(frame1),names(frame2))
  common_names <- subset(common, !grepl(paste(c("*sigma*","*scale*","*offset*"), collapse="|"),common))
  compareframe <- rbind(frame1[fits[1],common_names], frame2[fits[2],common_names])
  # plot
  pl <- plotPars(compareframe, tol = steptol)
  ggsave(file.path(.plotFolder, paste0("00x-ComparePars_", model1, "[",fits[1], "]VS", model2, "[",fits[2], "].pdf")), pl, width = 30, height = 10, device = cairo_pdf)
  print(pl)
}

#' Observables plot
#'
#' @param runname 
#' @param fitnumber
#' @param fitvalue
#'
#' @return
#' @export
#'
#' @examples
PlotObservables <- function(fitnumber = step, value = fitvalue, errors = "blotit"){
  cairo_pdf(filename = file.path(.plotFolder, paste0("003-Observables_step", fitnumber, "_", value, ".pdf")), onefile = TRUE, width = 16, height = 6) #pdf öffnen
  names_plot <-  names(observables)
  prediction <- prd0(times, bestfit)

  data_plot <- subset(as.data.frame(mydata), name %in% names_plot)
  prediction_plot <- subset(as.data.frame(prediction, data = mydata), name %in% names_plot) %>% as.data.table()
  prediction_plot[name != "HGF_au" & time > 180, value := NA]
  prediction_plot <- prediction_plot[!is.na(value)]
  
  prediction_plot$HGF <- factor(prediction_plot$HGF)
  data_plot$HGF <- factor(data_plot$HGF)

    P <- ggplot(prediction_plot, aes(x = time, y = value)) +
      facet_wrap(~name, scales = "free") +
      geom_line(size = 1) + 
      # scale_linetype_manual(values = c("dashed", "solid"), labels = c("0", "40") )+
      geom_point(data = data_plot, size = 2) + 
      theme_dMod(base_size = 18) + scale_color_dMod() + 
      theme(legend.position = "top", legend.key.size = unit(0.6,"cm")) + 
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_line(colour = "grey97"), 
            panel.grid.minor = element_line(colour = "grey97"), 
            panel.background = element_blank()) +
      xlab("time [min]") +
      ylab(paste0("Conc. [a.u.]"))
    
    if(errors == "blotit") P <- P + geom_errorbar(data = data_plot, aes(ymin = value - sigma, ymax = value + sigma), size = 0.7)
    if(errors == "errmodel") P <- P + geom_ribbon(aes(ymin = value - sigma, ymax = value + sigma), lty = 0, alpha = 0.1)
    
    print(P)

  dev.off() #pdf schließen
  
}

#' DR plot
#'
#' @param runname 
#' @param fitnumber
#' @param fitvalue
#'
#' @return
#' @export
#'
#' @examples
PlotObsDR <- function(fitnumber = step, value = fitvalue, errors = "errmodel"){
  cairo_pdf(filename = file.path(.plotFolder, paste0("003-ObsDR_step", fitnumber, "_", value, ".pdf")), onefile = TRUE, width = 16, height = 11) #pdf öffnen
  names_plot <-  names(observables)
  prediction <- prd0(times, bestfit)
  
  data_plot <- subset(as.data.frame(mydata), name %in% names_plot & exp.type == "DR" & time == 10 & HGF != 0)
  targets <- unique(data_plot$name)
  prediction_plot <- subset(as.data.frame(prediction, data = mydata, errfn = e), name %in% targets & exp.type == "DR" & time == 10 & HGF != 0)
  
  # prediction_plot$HGF <- factor(prediction_plot$HGF)
  prediction_plot$diet <- factor(prediction_plot$diet)
  prediction_plot$condition <- factor(prediction_plot$condition)
  # data_plot$HGF <- factor(data_plot$HGF)
  data_plot$diet <- factor(data_plot$diet)
  data_plot$condition <- factor(data_plot$condition)
  
  P <- ggplot(prediction_plot, aes(x = HGF, y = value, color = diet, fill = diet)) +
    facet_wrap(~name, scales = "free") +
    geom_smooth(method = "loess", formula = y ~x, se = FALSE, size = 1) + 
    geom_point(data = data_plot, size = 2) + 
    theme_dMod(base_size = 18) + scale_color_DR() + scale_fill_DR() +
    theme(legend.position = "top", legend.key.size = unit(0.6,"cm")) + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_line(colour = "grey97"), 
          panel.grid.minor = element_line(colour = "grey97"), 
          panel.background = element_blank()) +
    scale_x_log10(breaks = c(0.1, 1, 10, 100), labels = c(0.1, 1, 10, 100)) +
    annotation_logticks(mid = unit(0.1, "cm"), sides = "b", alpha = 0.5) +
    scale_y_continuous(limits = c(-0.3,NA)) +
    xlab("HGF dose [ng/ml]") +
    ylab(paste0("Conc. [a.u.]"))
  
  if(errors == "blotit") P <- P + geom_errorbar(data = data_plot, aes(ymin = value - sigma, ymax = value + sigma), size = 0.7)
  if(errors == "errmodel") P <- P + geom_ribbon(aes(ymin = value - sigma, ymax = value + sigma), lty = 0, alpha = 0.1)
  
  print(P)
  
  dev.off() #pdf schließen
  
}


#' States plot
#'
#' @param runname 
#' @param fitnumber
#' @param fitvalue
#'
#' @return
#' @export
#'
#' @examples
PlotStates <- function(runname = run, fitnumber = step, value = fitvalue){
  cairo_pdf(filename = file.path(.plotFolder, paste0("004-States_step", fitnumber, "_", value, ".pdf")), onefile = TRUE, width = 16, height = 12) #pdf öffnen
  names_plot <-  reactions$states
  prediction <- prd0(times, bestfit)
  
  prediction_plot <- subset(as.data.frame(prediction, data = mydata), name %in% names_plot & exp.type == "TC")
  prediction_plot$HGF <- factor(prediction_plot$HGF)
  prediction_plot$diet <- factor(prediction_plot$diet)
  prediction_plot$condition <- factor(prediction_plot$condition)
  
  P <- ggplot(prediction_plot, aes(x = time, y = value, color = condition, linetype = HGF)) +
    facet_wrap(~name, scales = "free_y") +
    geom_line(size = 1) + 
    scale_linetype_manual(values = c("dashed", "solid"), labels = c("0", "40") )+
    theme_dMod(base_size = 18) + scale_color_DIET() + scale_fill_dMod() +
    theme(legend.position = "top", legend.key.size = unit(0.6,"cm")) + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_line(colour = "grey97"), 
          panel.grid.minor = element_line(colour = "grey97"), 
          panel.background = element_blank()) +
    xlab("time [min]") +
    ylab(paste0("Conc. [a.u.]"))
  
  print(P)

  dev.off() #pdf schließen
}

#' States DR plot
#'
#' @param runname 
#' @param fitnumber
#' @param fitvalue
#'
#' @return
#' @export
#'
#' @examples
PlotStatesDR <- function(runname = run, fitnumber = step, value = fitvalue){
  cairo_pdf(filename = file.path(.plotFolder, paste0("004-StatesDR_step", fitnumber, "_", value, ".pdf")), onefile = TRUE, width = 16, height = 16) #pdf öffnen
  names_plot <-  reactions$states
  prediction <- prd0(times, bestfit)
  
  prediction_plot <- subset(as.data.frame(prediction, data = mydata), name %in% names_plot & time == 10 & HGF != 0)
  prediction_plot$diet <- factor(prediction_plot$diet)
  prediction_plot$condition <- factor(prediction_plot$condition)
  
  P <- ggplot(prediction_plot, aes(x = HGF, y = value, color = diet)) +
    facet_wrap(~name, scales = "free") +
    geom_line(size = 1) + 
    theme_dMod(base_size = 18) + scale_color_DR() + scale_fill_dMod() +
    theme(legend.position = "top", legend.key.size = unit(0.6,"cm")) + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_line(colour = "grey97"), 
          panel.grid.minor = element_line(colour = "grey97"), 
          panel.background = element_blank()) +
    scale_x_log10(breaks = c(0.1, 1, 10, 100), labels = c(0.1, 1, 10, 100)) +
    annotation_logticks(mid = unit(0.1, "cm"), sides = "b", alpha = 0.5) +
    xlab("HGF dose [ng/ml]") +
    ylab(paste0("Conc. [a.u.]"))
  
  print(P)
  
  dev.off() #pdf schließen
}

#' Profiles plot
#'
#' @param profiles
#' @param runname 
#' @param fitnumber
#' @param value
#'
#' @return
#' @export
#'
#' @examples
PlotProfiles <- function(myprofiles = profiles, runname = run, fitnumber = step, value = fitvalue, profpars = NULL){
  if(!is.null(profpars)) myprofiles <- subset(myprofiles, whichPar %in% profpars)
  pl <- plotProfile(myprofiles, mode == "data")
  if(is.null(profpars)){
    ggsave(file.path(.plotFolder, paste0("006-Profiles_", runname, ".run_step", fitnumber, "_", value, ".pdf")), pl, device = cairo_pdf, width = 90, height = 90, units = "cm")
  } else print(pl)
}


#' Flux plot
#'
#' @param runname 
#' @param fitnumber
#' @param fitvalue
#'
#' @return
#' @export
#'
#' @examples
PlotFluxes <- function(runname = run, fitnumber = step, value = fitvalue){
  mysample <- c("T47D","MCF7", "MDA231")
  myproteins <- c("R1", "R2","R3", "R1R1", "R1R2", "R1R3", "R2R3", "pcRAF", "pMEK","ppMEK", "pERK", "ppERK", "pAKT")
  cairo_pdf(filename = file.path(.plotFolder, paste0("005-Fluxes_", runname, ".run_step", fitnumber, "_", value, ".pdf")), onefile = TRUE, width = 15, height = 10)
  
  for (i in myproteins) {
    cat("plot",i,"\n")
    
    out <- attr(plotFluxes(bestfit, prd, times, getFluxes(reactions)[[i]], nameFlux = c("Fluxes")), "out")
    out$condition <- str_replace(out$condition, pattern = "no_inhibitor", replacement = "control")
    out <- out %>% separate(col = "condition", into = c("inhibitor", "sample", "ligand")) %>% rename(Flux = name)
    out1 <- subset(out, inhibitor == "control") %>% mutate(protein = i)

    out_plot <- out1
    out_plot$inhibitor <- factor(out_plot$inhibitor, levels = c("control", "pertuzumab", "trastuzumab", "erlotinib", "cetuximab", "lumretuzumab", "erlotinib_pertuzumab", "erlotinib_trastuzumab", "trastuzumab_erlotinib", "pertuzumab_erlotinib", "", "perturbation"))
    out_plot$sample <- factor(out_plot$sample, levels = c("T47D", "MCF7", "MDA231"))
    
    plot <- ggplot(subset(out_plot, time <= 250 & time >= 0), aes(x = time, y = value, group = Flux, fill = Flux, log = "y")) + 
      facet_grid(sample~ligand, scales ="free") + 
      scale_fill_manual(values = c(blue2red(out_plot$Flux %>% levels() %>% length()))) +
      geom_density(stat = "identity", position = "stack", alpha = 0.8, color = "black", size = 0.2) + 
      theme_dMod(base_size = 8) +
      theme(legend.position = "top", legend.key.size = unit(0.3,"cm"), legend.key.width = unit(0.3,"cm")) + #
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_line(colour = "grey97"), 
            panel.grid.minor = element_line(colour = "grey97"), 
            panel.background = element_blank()) +
      xlab("time [min]") +
      ylab(paste0(i, " concentration [a.u.]")) +
      ggtitle(paste0("Fluxes for ",i))
    
    print(plot)
    
  }
  
  dev.off() #pdf schließen
}


#' Make predictions along the profile
#'
#' @param jobname job name on cluster
#'
#' @return
#' @export
#'
#' @examples
predict_array <- function (prd, times, pars = partable, whichpar = par, keep_names = NULL, FLAGverbose = FALSE, FLAGverbose2 = FALSE, FLAGbrowser = FALSE, ...) {
  if (FLAGverbose2) cat("Simulating", "\n")
  out <- lapply(1:nrow(pars), function(i) {
    if (FLAGverbose) cat("Parameter set", i, "\n")
    if (FLAGbrowser) browser()
    mypar <- pars[i,] %>% as.numeric()
    parval <- round(pars[i,][[whichpar]], digits = 2)
    names(mypar) <- names(pars)
    mypar <- as.parvec(mypar)
    prediction <- try(prd(times, mypar, deriv = FALSE, ...))
    if (inherits(prediction, "try-error")) {
      warning("parameter set ", i, " failed\n")
      return(NULL)
    }
    prediction <- imap(prediction, function(.x,.y){
      .x <- data.table(.x)
      if (!is.null(keep_names))
        .x[, (setdiff(names(.x), c(keep_names, "time"))) := NULL]
      .x[, `:=`(condition = .y, ParValue = parval)]
      .x
    })
    melt(rbindlist(prediction), variable.name = "name", value.name = "value", id.vars = c("time", "condition", "ParValue"))
  })
  if (FLAGverbose2) cat("postprocessing", "\n")
  out <- rbindlist(out[!is.null(out)])
  
  pars <- cf_parf_getMeta(pars)
  if (!is.null(pars)){
    pars <- data.table(pars)[, `:=`(ParValue = 1:length(fitrank))]
    out <- merge(pars, out, by = "ParValue")
    out$ParValue <- NULL
  }
  out
}


#' Plot Simulations along Profiles
#'
#' @param jobname job name on cluster
#'
#' @return
#' @export
#'
#' @examples

PlotArray <- function (myPar, direction, ..., myprofiles = profiles, covtable = condition.grid, nsimus = 4, myprd = prd, mytimes = times) {
  library(viridis)

  # select subframe from profiles
  mysub <- myprofiles %>% as.data.table() %>% .[whichPar == myPar, ]
  mysub[, ID := 1:nrow(mysub)]
  # get ID of bestfit (constraint is 0 for bestfit)
  bestID <- mysub[constraint == 0.00]$ID
  if(direction == "up") mysubF <- mysub[ID >= bestID]  
  if(direction == "down") mysubF <- mysub[ID <= bestID]
  
  # select rows according to simulation number
  partable <- mysubF[seq(1, nrow(mysubF), (round(nrow(mysubF)/nsimus)))]
  
  # remove non_parameter names
  no_pars <- c("value", "constraint", "stepsize", "gamma", "whichPar", "data", "condition_obj", "AIC", "BIC", "prior", "ID")
  partable <- partable %>% .[, (no_pars) := NULL]
  
  predictionDT <- predict_array(prd = prd, times = mytimes, pars = partable, whichpar = myPar)
  out_plot <- copy(predictionDT)
  out_plot <- merge(out_plot, covtable, by = "condition")
  out_plot <- out_plot[...]

  P <- ggplot(out_plot , aes(x = time, y = value, group = ParValue, color = ParValue)) +
    facet_grid(name~condition, scales = "free_y") +
    geom_line(size = 1) + 
    # geom_point(data = out_data, color = "black") +
    theme_dMod(base_size = 18) + scale_color_viridis_c() +
    theme(legend.position = "top", legend.key.size = unit(0.6,"cm")) + 
    theme(axis.line = element_line(colour = "black"), 
          panel.grid.major = element_line(colour = "grey97"), 
          panel.grid.minor = element_line(colour = "grey97"), 
          panel.background = element_blank()) +
    xlab("time") +
    ylab(paste0("value"))
  print(P)
}


# Figure2
if(FALSE){
  
  data_list <- mydata
  names_plot <-  names(observables)[1:7] #phosphodata
  prediction <- prd(times, bestfit)
  ligands <- c("HRG","EGF")
  
  # New facet label names for name variable
  name.labs <- c("pEGFR","pERBB2","pERBB3","pAKT", "pcRAF", "pMEK","pERK")
  names(name.labs) <- c("pEGFR_obs","pERBB2_obs","pERBB3_obs","pAKT_obs", "pcRAF_obs", "pMEK_obs","pERK_obs")
  
  data_plot <- subset(as.data.frame(data_list), name %in% names_plot & ligand %in% ligands)
  prediction_plot <- subset(as.data.frame(prediction, data = data_list, errfn = e), name %in% names_plot & ligand %in% ligands)
  prediction_plot$ligand <- factor(prediction_plot$ligand, levels = c("HRG","EGF"))
  prediction_plot$condition <- factor(prediction_plot$condition, levels = condition_levels)
  prediction_plot$sample <- factor(prediction_plot$sample, levels = c("T47D", "MCF7"))
  
  P <- ggplot(prediction_plot , aes(x = time, y = value, group = condition, color = inhibitor, fill = inhibitor, lty = inhibitor)) +
    facet_grid(name~sample+ligand, scales = "free", labeller = labeller(name = name.labs)) + #"fixed"
    geom_ribbon(aes(ymin = value - sigma, ymax = value + sigma), lty = 0, alpha = 0.05) +
    geom_line(size = 0.4) + 
    geom_point(data = data_plot, size = 0.4) + 
    theme_SBA() + scale_color_SBA() + scale_fill_SBA() + scale_linetype_manual(values = c(2,1,1,1,1,1)) +
    theme(legend.position = "top", legend.key.size = unit(0.5,"cm")) + 
    guides(colour = guide_legend(nrow = 1)) +
    xlab("time [min]") +
    ylab(paste0("signal [a.u.]"))
  P
  
  ggsave(file.path(.plotFolder, paste0("007-Figure2_", run, ".run_step", step, "_", fitvalue, ".pdf")), device = cairo_pdf, width = 16, height = 21, units = "cm")
}


# -------------------------------------------------------------------------#
# 3 Profiles ----
# -------------------------------------------------------------------------#

#' Run profiles on cluster
#'
#' @param jobname job name on cluster
#' @param recover 
#' @param nodetype 
#'
#' @return
#' @export
#'
#' @examples
SendProfiles <- function(jobname = profile.name, recover = T, nodetype = "best", method = "integrate"){
  setwd(.currentwd)
  # define node type
  if(nodetype == "best") mycores <- "16:best"
  if(nodetype == "standard") mycores <- "16"
  if(nodetype == "bestplus") mycores <- "16:bestplus"
  mymethod <- method
  
  # define parameter distribution on nodes
  n_pars <- length(outerpars)
  profile_runs <- 1:ceiling(n_pars/16)
  
  start <- 16
  stop_vec <- 16
  while (start <= (n_pars)) {
    start <- start + 16
    stop_vec <- c(stop_vec, start)
  }
  stop_vec[length(stop_vec)] <- n_pars
  
  start_vec <- c(1, stop_vec+1)
  start_vec <- head(start_vec, -1)
  
  # make object accessible to runbg_bwfor which looks in global environment
  assign("start_vec", start_vec, envir=globalenv())
  assign("stop_vec", stop_vec, envir=globalenv())
  assign("mymethod", mymethod, envir=globalenv())
  
  # send jobs 
  setwd(.modelFolder)
  for (i in profile_runs) {
    assign("i", i, envir=globalenv())
    assign(paste0(jobname, i), runbg_bwfor({
      profile(obj, 
              pars = bestfit, 
              whichPar = (start_vec[i]):(stop_vec[i]), 
              cores = 16, 
              fixed=NULL, 
              method = mymethod)
    }, machine = "cluster", 
    filename = paste0(jobname, i), 
    nodes = 1, 
    cores = mycores, 
    walltime = "7:00:00",
    compile=FALSE, 
    recover = recover)
    )
    
    # make function globally availible
    assign(paste0(jobname, i), eval(parse(text = paste0(jobname, i))), envir=globalenv())
  }
  assign("profile_runs", profile_runs, envir=globalenv())
  setwd(.currentwd)
}


#' Get profiles
#'
#' @param jobname job name on cluster
#' @param fitnumber
#' @param runname
#'
#' @return
#' @export
#'
#' @examples
GetProfiles <- function(jobname = profile.name, fitnumber = step, runname = run){
  setwd(.currentwd)
  # get fits
  setwd(.profileFolder)
  profiles <- NULL
  print("Collecting results from")
  for (i in profile_runs) {
    run_i_check <-  paste0(jobname, i,"$check()")
    if(eval(parse(text=run_i_check)) == TRUE) {
      print(paste0("Node ", i))
      run_i <-  paste0(jobname, i,"$get()")
      eval(parse(text=run_i)) 
      myprofile <- .runbgOutput[[1]]
      profiles <- rbind(profiles, myprofile)
    }
    # print(myprofile$whichPar %>% unique())
  }
  setwd(.currentwd)
  
  # save
  profiles %>% head()
  saveRDS(profiles, file = file.path(.profileFolder, paste0("profiles_", runname, "_step", fitnumber, "_", fitvalue, ".rds")))
  
  # make profiles globally availible
  assign("profiles", profiles, envir=globalenv())
}


#' Purge profiles
#'
#' @param jobname job name on cluster
#'
#' @return
#' @export
#'
#' @examples
PurgeProfiles <- function(jobname = profile.name){
  for (i in profile_runs) {
    run_i <-  paste0(jobname, i,"$purge()")
    eval(parse(text=run_i)) 
  }
}


#' Plot Profiles together
#'
#' @param jobname job name on cluster
#'
#' @return
#' @export
#'
#' @examples
PlotProfiles2gether <- function(runname = run, step1 = 1, step2 = 2){
  system(paste0("find ", .profileFolder, "/ -name profiles_", runname, "_step", step1, "_*"))
  prof1 <- readRDS(file = file.path(.profileFolder, paste0("profiles_", runname, "_step", step1, "_*")))
  prof2 <- readRDS(file = file.path(.profileFolder, paste0("profiles_", runname, "_step", step2, "_", fitvalue, ".rds")))
  myproflist <- list(prof1, prof2)
  pl <- plotProfile(myproflist, mode == "data")
  ggsave(file.path(.plotFolder, paste0("006-ProfilesCompare_", runname, ".run_step", step1, "AND", step2, ".pdf")), pl, device = cairo_pdf, width = 90, height = 90, units = "cm")
  
}

# -------------------------------------------------------------------------#
# 4 L1 Analysis ----
# -------------------------------------------------------------------------#

#' Run L1 fits on cluster
#'
#' @param jobname job name on cluster
#' @param recover 
#' @param nodetype 
#'
#' @return
#' @export
#'
#' @examples
SendL1Fits <- function(jobname = fit.name, recover = T, mypartition = "multi", lambdas = mylambdas){
  setwd(.currentwd)

  #send fits
  setwd(.modelFolder)
  vec <- lambdas
  assign("vec", vec, envir=globalenv())
  for(i in 1:length(vec)){
    assign("i", eval(parse(text = i)), envir=globalenv())
    Run.name <- paste0(jobname,str_pad(i,2,pad="0"))
    assign(Run.name,runbg_bwfor_slurm({
      mclapply(1:16, function(j){
        # trustL1 fit
        mypar <- ini + rnorm(length(ini), mean=0, sd=3)
        outL1 <-trustL1(obj, mypar, mu = pouterL1, lambda = exp(vec[i]), rinit = .1, rmax = 10, fixed = c(lambda = vec[i]), iterlim = 40000,
                        parupper = 12, parlower = -12)
        # fix L1 pars below threshold to 0
        myfixpars <- c()
        myopt <- outL1$argument
        for(par in names(myopt)[which(grepl("L1", names(myopt)))]){
          if(myopt[[par]]**2 < 0.001**2){myfixpars <- c(myfixpars, par); myopt[par] <- 0}
        }
        # refit remaining pars
        if(!is.null(myfixpars)){
          ub <- structure(rep(0,length(myfixpars)),names=myfixpars)
          lb <- structure(rep(0,length(myfixpars)), names=myfixpars)}  else{ub <- Inf; lb <- -Inf}
        
        myfit <- dMod::trust(obj, parinit = myopt, rinit = 0.1, rmax = 10,
                             parupper = ub, parlower = lb, fixed=c(lambda=-3),
                             iterlim = 20000) 
        # don't attach attributes (seems to make the output file big)
        # attr(myfit,"data") <- NULL
        attr(myfit,"prior") <- NULL
        attr(myfit,"class") <- NULL
        attr(myfit,"env") <- NULL
        # output file
        myfit
      }, mc.cores=16)
    }, machine = "cluster", partition = mypartition, filename = Run.name,
    nodes = 50, cores = 16, walltime = "20:00:00", compile=FALSE, recover = recover, password = mypassword)
    )
    
    # make function globally availible
    assign(Run.name, eval(parse(text = Run.name)), envir=globalenv())
  }
  setwd(.currentwd)
  
  # Message
  if(recover == F) print(paste0("L1 fits have been successfully uploaded to the cluster under the name ", jobname, "XX"))
  if(recover == T) print(paste0("The job ", jobname, " has been successfully recovered."))
}

SendL1FitsWOrefit <- function(jobname = fit.name, recover = T, nodetype = "standard", lambdas = mylambdas){
  setwd(.currentwd)
  # define node type
  if(nodetype == "best") mycores <- "16:best"
  if(nodetype == "standard") mycores <- "16:standard"
  if(nodetype == "bestplus") mycores <- "16:bestplus"
  
  #send fits
  setwd(.modelFolder)
  vec <- lambdas
  assign("vec", vec, envir=globalenv())
  for(i in 1:length(vec)){
    assign("i", eval(parse(text = i)), envir=globalenv())
    Run.name <- paste0(jobname,str_pad(i,2,pad="0"))
    assign(Run.name,runbg_bwfor({
      mclapply(1:16, function(j){
        # trustL1 fit
        mypar <- ini + rnorm(length(ini), mean=0, sd=3)
        outL1 <-trustL1(obj, mypar, mu = pouterL1, lambda = exp(vec[i]), rinit = .1, rmax = 10, fixed = c(lambda = vec[i]), iterlim = 40000,
                        parupper = 12, parlower = -12)
        outL1
      }, mc.cores=16)
    }, machine = "cluster2", filename = Run.name,
    nodes = 5, cores = mycores, walltime = "100:00:00", compile=FALSE, recover = recover)
    )
    
    # make function globally availible
    assign(Run.name, eval(parse(text = Run.name)), envir=globalenv())
  }
  setwd(.currentwd)
  
  # Message
  if(recover == F) print(paste0("L1 fits w/o refit have been successfully uploaded to the cluster under the name ", jobname, "XX"))
  if(recover == T) print(paste0("The job ", jobname, " has been successfully recovered."))
}


#' Get L1 cluster jobs
#'
#' @param runname 
#' @param jobname
#'
#' @return
#' @export
#'
#' @examples
GetL1Fits <- function(jobname = fit.name, lambdas = mylambdas){
  setwd(.currentwd)
  setwd(file.path(.fitFolder))
  vec <- lambdas
  fitlist_L1 <- NULL
  # check and get what's ready
  print("Collecting results from")
  for (i in 1:length(vec)) {
    Run.name <- paste0(jobname,str_pad(i,2,pad="0"))
    run_i_check <-  paste0(Run.name,"$check()")
    # if(eval(parse(text=run_i_check)) == TRUE) {
    print(paste0("Lambda ", vec[as.numeric(i)]))
    run_i <-  paste0(Run.name,"$get()")
    eval(parse(text=run_i))
    
    node <- .runbgOutput %>% length() # usually 10
    nodelist <- as.data.table(do.call(rbind, lapply(1:node, function(k){
      
      do.call(rbind, lapply(1:16, function(j){
        # print(paste0("core (j) ",j))
        # if(!has_error(.runbgOutput[[j]],silent = TRUE)){
        myfit <- .runbgOutput[[k]][[j]]
        if(!inherits(myfit, "try-error")){
          if(is.null(myfit$error)){
            if(!myfit$value %>% is.null){
              if(!myfit$value %>% is.na){
                c(i=as.numeric(i), j=j, node=k, lambda = vec[as.numeric(i)],value = myfit$value,
                  data = attr(myfit, "data"), 
                  iterations = myfit$iterations, 
                  converged = myfit$converged,
                  nr_non_zero = length(which(grepl("L1", names(myfit$argument)) & myfit$argument**2 > 0.0001**2)) %>% round(),
                  AIC = attr(myfit, "data") + 2*length(which(grepl("L1", names(myfit$argument)) & myfit$argument**2 > 0.0001**2)),
                  BIC = attr(myfit, "data") + log(dim(do.call(rbind,mydata))[1])*length(which(grepl("L1", names(myfit$argument)) & myfit$argument**2 > 0.0001**2)),
                  myfit$argument)
              }else NULL
              
            } else NULL
          } else NULL
        } else NULL
        # } else print(paste0("Fit Error for lambda ",i," , node ",k," core ", j))
        # } else print(paste0("Try Error for lambda ",i," , node ",k," core ", j))
      }))
      
    })))
    fitlist_L1 <- rbind(fitlist_L1, nodelist)
    # }
  }
  setwd(.currentwd)
  fitlist_L1 <- fitlist_L1[order(fitlist_L1$BIC),]
  saveRDS(fitlist_L1, file = file.path(.fitFolder, paste0("fitlist_L1_", min(fitlist_L1$lambda), ".", max(fitlist_L1$lambda), "by_", abs(mylambdas[1]-mylambdas[2]), ".rds")))
  # make fitlist_L1 globally availible
  assign("fitlist_L1", fitlist_L1, envir=globalenv())
}


#' Plot L1 Results
#'
#' @param runname 
#' @param jobname
#'
#' @return
#' @export
#'
#' @examples
PlotL1Fits <- function(flist = fitlist_L1, orderby = 0, L1 = TRUE){
  library(cowplot)
  L1pars <- names(flist)[which(grepl("_WD", names(flist)))]
  # L1pars <- setdiff(L1pars, grep("_R", L1pars, value = T))
  Otherpars <- setdiff(names(flist), c(L1pars, grep("scale|offset|sigma", names(flist), value = T)))[-1:-11]
  
  if(L1 == FALSE) { mypars <- Otherpars
  } else mypars <- L1pars
  
  out <- do.call(rbind, lapply(unique(flist$lambda), function(myi){
    fitlist_myi <- subset(flist, lambda==myi)
    fitlist_myi <- fitlist_myi[order(fitlist_myi$BIC),]
    # select best 20% of fits
    fitlist_myi <- fitlist_myi[1:round(dim(fitlist_myi)[1]*0.2),]
    do.call(rbind, lapply(mypars, function(L1par){ #
      values <- fitlist_myi[,get(L1par)]
      endvalue <- 0
      for (v in values){
        if(v < -0.01){endvalue <- endvalue + v}
        if(v > 0.01){endvalue <- endvalue + v}
      }
      endvalue <- endvalue/length(values)
      data.frame(value=endvalue, parameter=L1par, index=myi, lambda=unique(fitlist_myi$lambda))#
    }))
  }))
  
  out$parameter <- factor(out$parameter, levels = out %>% filter(lambda == orderby) %>% arrange(value) %>% .[,2])
  
  P1 <- ggplot(out, aes(x=lambda, y=parameter, color=value, fill=value)) + geom_tile(colour = NA, width = 1, height = 1) + 
    theme_bw(base_size = 7) + ylab("parameter") +
    scale_color_gradient2(low="red", mid="black", high="green") + #turquoise1
    scale_fill_gradient2(low="red", mid="black", high="green")+
    labs(x = expression("Regularization Strength "*lambda), y = "", fill = expression(log(r[i])), color = expression(log(r[i]))) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  out2 <- do.call(rbind, lapply(unique(flist$lambda), function(myi){
    sub <- subset(flist, lambda==myi)
    sub <- sub[order(sub$BIC),]
    # select number of Likelihood Values to plot
    sub <- sub[1:round(dim(sub)[1]*0.2),]
    data.frame(value=sub$value, BIC=sub$BIC, data=sub$data, nr_non_zero = sub$nr_non_zero*10+500, order=1:length(sub$value), index=myi, lambda=sub$lambda)
  }))
  P2 <- ggplot(subset(wide2long(out2, keep=c(5,6,7)), name=="BIC"), aes(x=(lambda+order/150), y=value, color=name)) + geom_point(shape=20, size=0.2) + 
    theme_bw(base_size = 6) + ylab("BIC") + xlab(expression("Regularization Strength "*lambda)) + theme(legend.position = "none")
  
  
  mylambdas <- out$lambda %>% unique() %>% sort()
  
  if(L1 == TRUE){ myname <- "L1_summary_"
  } else myname <- "L1_summary_others_"
  plotname <- paste0(myname, min(out$lambda), ".", max(out$lambda), "by_", (mylambdas[1]-mylambdas[2]), ".pdf")
  pdf(file.path(.plotFolder, plotname), height=6, width=20/2.54, useDingbats=FALSE) #25/2.54 190821_Combinedmodel13.1/L1 analysis/
  P <- ggdraw() + #theme_nothing(base_size = 16) +
    draw_plot(P1, 0, .2, 1, .8)  +
    draw_plot(P2, .115, 0, .803, .2)
  print(P)
  dev.off()
  
}


# -------------------------------------------------------------------------#
# 5 Themes ----
# -------------------------------------------------------------------------#

# Theme Systems Biology and Applications

theme_SBA <- function (base_size = 7, base_family = "") 
{
  colors <- list(medium = c(gray = "#737373", red = "#F15A60", 
                            green = "#7AC36A", blue = "#5A9BD4", orange = "#FAA75B", 
                            purple = "#9E67AB", maroon = "#CE7058", magenta = "#D77FB4"), 
                 dark = c(black = "#010202", red = "#EE2E2F", green = "#008C48", 
                          blue = "#185AA9", orange = "#F47D23", purple = "#662C91", 
                          maroon = "#A21D21", magenta = "#B43894"), 
                 light = c(gray = "#CCCCCC", red = "#F2AFAD", green = "#D9E4AA", blue = "#B8D2EC", 
                           orange = "#F3D1B0", purple = "#D5B2D4", maroon = "#DDB9A9", 
                           magenta = "#EBC0DA"))
  gray <- colors$medium["gray"]
  black <- colors$dark["black"]
  theme_bw(base_size = base_size, base_family = base_family) + 
    theme(line = element_line(colour = "black"), 
          rect = element_rect(fill = "white",colour = NA), text = element_text(colour = "black"), 
          axis.text = element_text(size = rel(1), colour = "black"), 
          axis.text.x = element_text(margin = unit(c(2, 4, 2, 4), "mm")), 
          axis.text.y = element_text(margin = unit(c(4, 2, 4, 2), "mm")), 
          axis.title.x = element_text(margin = margin(t = -5, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0)),
          axis.ticks = element_line(colour = "black"), 
          axis.ticks.length = unit(1, "mm"),
          legend.key = element_rect(colour = NA), 
          panel.border = element_rect(colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          strip.background = element_rect(fill = "white", colour = NA), strip.text = element_text(size = rel(1)))
  
}

dMod_colors <- c("#000000", "#C5000B", "#0084D1", "#579D1C", "#FF950E", "#4B1F6F", "#CC79A7","#006400", "#F0E442", "#8B4513", rep("gray", 100))
SBA_colors <- c("#000000","#FFD942", "#FF950E","#0084D1","#0147A6","#006400", rep("gray", 100))

#' @rdname scale_color_DIET
scale_color_DIET <- function(...) {
  DIET_vals <- setNames(c("#000000", "#8b21ab", "#668e18", "#f39200"), c("0_SD_TC", "40_SD_TC", "0_WD_TC", "40_WD_TC"))
  scale_color_manual(..., values = DIET_vals)
}

#' @rdname scale_fill_DIET
scale_fill_DIET <- function(...) {
  DIET_vals <- setNames(c("#000000", "#8b21ab", "#668e18", "#f39200"), c("0_SD_TC", "40_SD_TC", "0_WD_TC", "40_WD_TC"))
  scale_fill_manual(..., values = DIET_vals)
}

#' @rdname scale_color_DR
scale_color_DR <- function(...) {
  DR_vals <- setNames(c("#8b21ab", "#f39200"), c("SD", "WD"))
  scale_color_manual(..., values = DR_vals)
}
#' @rdname scale_fill_DR
scale_fill_DR <- function(...) {
  DR_vals <- setNames(c("#8b21ab", "#f39200"), c("SD", "WD"))
  scale_fill_manual(..., values = DR_vals)
}
scale_color_SBA <- function (...) 
{
  scale_color_manual(..., values = SBA_colors)
}

scale_fill_SBA <- function (...) 
{
  scale_fill_manual(..., values = c("transparent","#FFD942", "#FF950E","#0084D1","#0147A6","#006400", rep("gray", 100)))
}

condition_levels <- c( "cetuximab_T47D_TGF",    "cetuximab_T47D_BTC",    "cetuximab_T47D_HRG",   "cetuximab_T47D_EGF",  
                       "cetuximab_MCF7_TGF",    "cetuximab_MCF7_BTC",    "cetuximab_MCF7_HRG",    "cetuximab_MCF7_EGF", 
                       "erlotinib_T47D_TGF",    "erlotinib_T47D_BTC"  ,  "erlotinib_T47D_HRG" ,   "erlotinib_T47D_EGF"  ,  
                       "erlotinib_MCF7_EGF" ,   "erlotinib_MCF7_TGF"   , "erlotinib_MCF7_BTC"  ,  "erlotinib_MCF7_HRG" ,
                       "pertuzumab_T47D_TGF" ,  "pertuzumab_T47D_BTC"   ,"pertuzumab_T47D_HRG"  , "pertuzumab_T47D_EGF" ,   
                       "pertuzumab_MCF7_TGF"  , "pertuzumab_MCF7_BTC",   "pertuzumab_MCF7_HRG"   ,"pertuzumab_MCF7_EGF"  ,
                       "trastuzumab_T47D_HRG"  ,"trastuzumab_T47D_EGF",  "trastuzumab_T47D_TGF",  "trastuzumab_T47D_BTC" ,
                       "trastuzumab_MCF7_TGF",  "trastuzumab_MCF7_BTC" , "trastuzumab_MCF7_HRG" , "trastuzumab_MCF7_EGF"  , 
                       "lumretuzumab_T47D_TGF", "lumretuzumab_T47D_BTC" ,"lumretuzumab_T47D_HRG" ,"lumretuzumab_T47D_EGF" ,
                       "lumretuzumab_MCF7_TGF" ,"lumretuzumab_MCF7_BTC", "lumretuzumab_MCF7_HRG", "lumretuzumab_MCF7_EGF",
                       "no_inhibitor_T47D_BTC", "no_inhibitor_T47D_HRG" ,"no_inhibitor_T47D_EGF" ,"no_inhibitor_T47D_TGF",
                       "no_inhibitor_MCF7_TGF" ,"no_inhibitor_MCF7_BTC" ,"no_inhibitor_MCF7_HRG" ,"no_inhibitor_MCF7_EGF")

mypassword <- "'OHZfrs91!!'"

# Exit ----

