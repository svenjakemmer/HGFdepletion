# -------------------------------------------------------------------------#
# 0 Calculate Profiles ----
# -------------------------------------------------------------------------#
#
# S005-CalcProfiles.R
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
# 1 Load bestfit ----
# -------------------------------------------------------------------------#
myframe <- readRDS(file.path(.fitFolder, "myframe.rds"))
step <- 1
bestfit <- as.parvec(myframe, step)

# bestfit <- readRDS(file.path(.fitFolder, "bestfit.rds"))

# -------------------------------------------------------------------------#
# 2 Calculate profiles on cluster ----
# -------------------------------------------------------------------------#
# .. 1 Distribute pars on nodes -----
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

# .. 2 Send jobs -----
setwd(.modelFolder)
for (i in profile_runs) {
  assign(paste0("job_profiles", i), runbg_bwfor({
    profile(obj, 
            pars = bestfit, 
            whichPar = (start_vec[i]):(stop_vec[i]), 
            # stepControl = list(stepsize = 1e-4, min = 1e-4, limit = 4000),  #stepControl = list(stepsize = 2e-2, min = 1e-3, limit = 4000)
            # limits = c(-6, 6), 
            # algoControl = list(reg = 0), 
            cores = 16, 
            fixed=NULL, 
            method = "integrate")
  }, machine = "cluster", 
  filename = paste0("job_profiles", i), 
  nodes = 1, 
  cores = "16:best", 
  walltime = "100:00:00",
  input = c("obj", "bestfit", "e", "start_vec", "stop_vec", "i"), 
  compile=FALSE, 
  recover = T)
  )
}
setwd(.currentwd)

# .. 3 check/get -----
# check single profile runs
job_profiles1$check()


# merge all profiles in "profiles"
setwd(.profileFolder)
profiles <- NULL
for (i in profile_runs) {
  run_i <-  paste0("job_profiles", i,"$get()")
  eval(parse(text=run_i)) 
  myprofile <- .runbgOutput[[1]]
  profiles <- rbind(profiles, myprofile)
}
setwd(.currentwd)

# .. 4 save/purge -----

saveRDS(profiles, file = file.path(.profileFolder, "profiles.rds"))

# purge all profiles
for (i in profile_runs) {
  run_i <-  paste0("job_profiles", i,"$purge()")
  eval(parse(text=run_i)) 
}

# plot profiles
myprofile[!grep("condition_obj", names(myprofile))]
plotProfile(profiles)


# Exit ----

