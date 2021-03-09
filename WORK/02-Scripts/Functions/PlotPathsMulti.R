PlotPaths <- function(profs=myprofiles, ..., whichPar, sort = FALSE, relative = TRUE, scales = "fixed", multi = TRUE, n_pars = 5) {
  
  if ("parframe" %in% class(profs)) 
    arglist <- list(profs)
  else
    arglist <- as.list(profs)
  
  
  if (is.null(names(arglist))) {
    profnames <- 1:length(arglist)
  } else {
    profnames <- names(arglist)
  }
  
  
  data <- do.call(rbind, lapply(1:length(arglist), function(i) {
    # choose a proflist
    proflist <- as.data.frame(arglist[[i]])
    parameters <- attr(arglist[[i]], "parameters")
    
    if (is.data.frame(proflist)) {
      whichPars <- unique(proflist$whichPar)
      proflist <- lapply(whichPars, function(n) {
        with(proflist, proflist[whichPar == n, ])
      })
      names(proflist) <- whichPars
    }
    
    if (is.null(whichPar)) whichPar <- names(proflist)
    if (is.numeric(whichPar)) whichPar <- names(proflist)[whichPar]
    
    subdata <- do.call(rbind, lapply(whichPar, function(n) {
      # matirx
      paths <- as.matrix(proflist[[n]][, parameters])
      values <- proflist[[n]][, "value"]
      origin <- which.min(abs(proflist[[n]][, "constraint"]))
      if (relative) 
        for(j in 1:ncol(paths)) paths[, j] <- paths[, j] - paths[origin, j]
      
      combinations <- expand.grid.alt(whichPar, colnames(paths))
      if (sort) combinations <- apply(combinations, 1, sort) else combinations <- apply(combinations, 1, identity)
      combinations <- submatrix(combinations, cols = -which(combinations[1,] == combinations[2,]))
      combinations <- submatrix(combinations, cols = !duplicated(paste(combinations[1,], combinations[2,])))
      
      
      
      
      path.data <- do.call(rbind, lapply(1:dim(combinations)[2], function(j) {
        data.frame(chisquare = values, 
                   name = n,
                   proflist = profnames[i],
                   combination = paste(combinations[,j], collapse = " - \n"),
                   x = paths[, combinations[1,j]],
                   y = paths[, combinations[2,j]])
      }))
      
      if(multi) path.data <- path.data %>% as.data.table %>% .[, partner := tstrsplit(as.character(combination), "\n", fixed=TRUE, keep = 2)]
                                                       
      
      return(path.data)
      
    }))
    
    return(subdata)
    
  }))

  data$proflist <- as.factor(data$proflist)

  if (relative)
    axis.labels <- c(expression(paste(Delta, "parameter 1")), expression(paste(Delta, "parameter 2")))  
  else
    axis.labels <- c("parameter 1", "parameter 2")
  
  
  data <- droplevels(subset(data, ...))
  
  suppressMessages(
    p <- ggplot(data, aes(x = x, y = y, group = interaction(name, proflist), color = name, lty = proflist)) + 
      facet_wrap(~combination, scales = scales) + 
      geom_path() + #geom_point(aes=aes(size=1), alpha=1/3) +
      xlab(axis.labels[1]) + ylab(axis.labels[2]) +
      scale_linetype_discrete(name = "profile\nlist") +
      scale_color_manual(name = "profiled\nparameter", values = dMod_colors)
  )
  if(multi){

    # determine strength of change
    data[, max.dev := max(c(abs(max(y)), abs(min(y)))), by = "partner"]
    setorder(data, name, -max.dev)
    # max.devis <- unique(data$max.dev)[1:n_pars]
    data[!(max.dev %in% unique(max.dev)[1:n_pars]), partner := "others"]

    data$combination <- as.factor(data$combination)
    data$partner <- factor(data$partner, levels = unique(data$partner))
    
    suppressMessages(
    p <- ggplot(data, aes(x = x, y = y, color = partner)) + 
      geom_path() + #geom_point(aes=aes(size=1), alpha=1/3) +
      xlab(paste0("log(", whichPar, ")")) + ylab("change of other paramters") +
      scale_linetype_discrete(name = "profile\nlist") +
      scale_color_manual(values = c(dMod_colors[2:(n_pars+1)], rep("gray", 100))) + theme_dMod() +
      theme(legend.position = c(0.8, 0.3),
            legend.title = element_blank(),
            legend.box.background = element_rect(colour = "black"),
            legend.key.size = unit(0.4, "cm"))
    )
  }
  
  attr(p, "data") <- data
  return(p)
  
}

PlotPathsMulti <- function(myprofiles, whichPars, npars = 5) {
  PlotList <- NULL
  for(i in 1:length(whichPars)){
    par <- whichPars[i]
    p <- PlotPaths(profs=myprofiles, whichPar = par, n_pars = npars)
    PlotList[[i]] <- p
  }
  cowplot::plot_grid(plotlist = PlotList)
}

expand.grid.alt <- function(seq1, seq2) {
  cbind(Var1=rep.int(seq1, length(seq2)), Var2=rep(seq2, each=length(seq1)))
}

loadEnvir <- function(envir = parent.frame()) ls(envir = envir)
