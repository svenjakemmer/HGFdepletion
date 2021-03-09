# -------------------------------------------------------------------------#
# 0 Fit model ----
# -------------------------------------------------------------------------#
#
# S004-FitModel.R
#
# [PURPOSE]
# 
# 
#
#
# [AUTHOR]
# Svenja Kemmer
#
# [Date]
# 2020-04-15 
#
# -------------------------------------------------------------------------#
# 0 local fit ----
# -------------------------------------------------------------------------#
# .. 1 single fit -----
print("start fit")
myfit <- trust(obj, ini, rinit=1, rmax=10, iterlim=50) #,blather=FALSE
print(paste("fit",myfit$converged,"with",myfit$iterations,"iterations to",myfit$value))
#obj_sep(myfit$argument)
para_plot <- myfit$argument
prediction <- (g*x*p)(times, para_plot,attach=TRUE)

print(plotCombined(prediction,  mydata))

# .. 2 multi start -----
out <- mstrust(obj, ini, rinit = 1, rmax = 10, iterlim = 200, #1000
               sd = 3, parupper = 12, parlower = -12, 
               cores = detectFreeCores(), fits = 10)

myframe <- as.parframe(out)
plotValues(myframe)
step <- 1
fitvalue <- myframe$value[step] %>% round(digits = 2)
bestfit <- as.parvec(myframe, 1)
prediction <- (g*x*p)(times, bestfit,attach=TRUE)
print(plotCombined(prediction,  mydata))#+aes(color=experiment)

saveRDS(myframe, file = file.path(.fitFolder, paste0("myframe.rds")))
PlotObservables()
PlotObsDR()
PlotStates()
# -------------------------------------------------------------------------#
# 1 mstrust on cluster ----
# -------------------------------------------------------------------------#

SendFits(recover = F)

# .. 1 Define bounds -----
lower = c(rep(-6, length(ini)))
lower[c(2,3,5,6,8,9)] <- -2
upper = c(rep(5, length(ini)))
# which(ini > 4.0) # which(names(ini)=="out_R3_basal")
upper[c(1)] <- 10

# .. 2 Send job -----
setwd(.modelFolder)
out_ms_constr <- runbg_bwfor({
  mstrust(obj, ini, rinit = 1, rmax = 10, iterlim=40000, 
          sd = 3, parupper = 12, parlower = -12, 
          # fterm = 1.5e-12, mterm = 1.5e-12,
          fits = 1*16, cores = 16, fixed = NULL) #
}, machine = "cluster", nodes = 32, cores = "16", walltime = "100:00:00", filename = "job_ms_constr",input = c("obj", "ini"), compile = FALSE, recover = F) #,"err"
setwd(.currentwd)

# Achtung beim Umrechnen der Grenzen von D2D in dMod! D2D verwendet log10 und dMod log - D2D Grenzen wären umgerechnet upr=6.907755, lwr=-11.51293

# .. 3 check/get -----
setwd(file.path(.fitFolder))
out_mstrust$check()
fits <- out_mstrust$get()
setwd(.currentwd)
fits <- unlist(fits, FALSE, use.names = FALSE)
fits %>% str1
myframe <- fits %>% as.parlist() %>% as.parframe() %>% add_stepcolumn()

myframe$value[c(1:20)]
head(myframe)

# .. 4 save/purge -----
run <- 1
saveRDS(myframe, file = file.path(.fitFolder, paste0("myframe_", run, ".run.rds")))

step <- 1
bestfit <- as.parvec(myframe, step)
fitvalue <- round(myframe[step]$value, digits = 2)
saveRDS(bestfit, file = file.path(.fitFolder, paste0("bestfit_step", step, ".rds")))
saveRDS(bestfit, file = file.path(.fitFolder, paste0("bestfit_", run, "_step", step, "_", fitvalue, ".rds")))

out_mstrust$purge()

# -------------------------------------------------------------------------#
# 2 Plots ----
# -------------------------------------------------------------------------#
if(TRUE){
  # waterfall plot
  pl61 <- plotValues(myframe[1:50])
  ggsave(file.path(.plotFolder, paste0("001-Waterfall_", run, ".run.pdf")), pl61, width = 10, height = 8, device = cairo_pdf)
  
  # parvalues
  common_names <- subset(names(myframe), !grepl(paste(c("*sigma*","*scale*","*offset*"), collapse="|"),names(myframe)))
  # common_names <- subset(names(myframe), grepl(paste(c("*L1*"), collapse="|"),names(myframe))) %>% c(.,"index", "value")
  pl62 <- plotPars(myframe[c(1:5),common_names])
  ggsave(file.path(.plotFolder, "002-ParValues.pdf"), pl62, width = 30, height = 10, device = cairo_pdf)
  
  # bestfit
  cairo_pdf(filename = file.path(.plotFolder, paste0("003-Observables_", run, ".run_step", step, ".pdf")), onefile = TRUE, width = 16, height = 35) #pdf öffnen
  names_plot <-  names(observables)
  prediction <- prd(times, bestfit)
  for (i in ligands) {
    # name Plot
    cat("plot",i,"\n")
    data_plot <- subset(as.data.frame(mydata), name %in% names_plot & ligand == i)
    prediction_plot <- subset(as.data.frame(prediction, data = mydata, errfn = e), name %in% names_plot & ligand == i)
    
    P <- ggplot(prediction_plot , aes(x = time, y = value, group = condition, color = sample, fill = sample)) +
      facet_grid(name~inhibitor, scales = "free_y") +
      geom_ribbon(aes(ymin = value - sigma, ymax = value + sigma), lty = 0, alpha = 0.1) +
      geom_line(size = 1) + 
      geom_point(data = data_plot, size = 2) + 
      theme_dMod(base_size = 18) + scale_color_dMod() + scale_fill_dMod() +
      theme(legend.position = "top", legend.key.size = unit(0.6,"cm")) + 
      theme(axis.line = element_line(colour = "black"), 
            panel.grid.major = element_line(colour = "grey97"), 
            panel.grid.minor = element_line(colour = "grey97"), 
            panel.background = element_blank()) +
      xlab("time [min]") +
      ylab(paste0("Conc. for ", i, " induction [a.u.]"))
    
    print(P)
  }
  
  dev.off() #pdf schließen
  
}


# Exit ----


