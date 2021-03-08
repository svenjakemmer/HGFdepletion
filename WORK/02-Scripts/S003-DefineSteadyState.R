# -------------------------------------------------------------------------#
# 0 Define Steady State 0 ----
# -------------------------------------------------------------------------#
#
# S003-DefineSteadyState.R
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
# 2020-04-02
#
try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)))
rm(list = ls(all.names = TRUE))
.dataFolder <- "../01-Data/"

# -------------------------------------------------------------------------#
# 1 Steady State equations ----
# -------------------------------------------------------------------------#

SS_vec <- c(
  
  MET = "k_MET_expression*(k_dephospho_MET+k_pMET_degradation)/(k_phospho_MET_basal*k_pMET_degradation)",
  pMET = "k_MET_expression/k_pMET_degradation",
  pERK = "ERK*k_phospho_ERK*pMET/(k_dephospho_pERK)",
  S6K = "k_dephospho_pAKT*k_dephospho_pERK*k_dephospho_pS6K*pS6K*(k_dephospho_MET*k_inhibit_AKT*pS6K + k_dephospho_MET + k_inhibit_AKT*k_pMET_degradation*pS6K + k_pMET_degradation)/(MET*k_phospho_MET_basal*(AKT*k_dephospho_pERK*k_phospho_AKT*k_phospho_S6K_AKT + ERK*k_dephospho_pAKT*k_inhibit_AKT*k_phospho_ERK*k_phospho_S6K_ERK*pS6K + ERK*k_dephospho_pAKT*k_phospho_ERK*k_phospho_S6K_ERK))",
  pAKT = "AKT*MET*k_phospho_AKT*k_phospho_MET_basal/(k_dephospho_pAKT*(k_dephospho_MET*k_inhibit_AKT*pS6K + k_dephospho_MET + k_inhibit_AKT*k_pMET_degradation*pS6K + k_pMET_degradation))"
  
)


SS_equations <- resolveRecurrence(SS_vec)

# .. Export SS_equations -----
saveRDS(SS_equations, file.path(.dataFolder, "03-SS_equations.rds"))

# -------------------------------------------------------------------------#
# 2 Steady State equations pMETpINSR ----
# -------------------------------------------------------------------------#

SS_vec <- c(
  
  pMET = "MET**2*k_phospho_MET_basal/(k_dephospho_MET + k_pMET_degradation)",
  pMETpINSR = "INSR*MET*k_phospho_MET_INSR_basal/(k_dephospho_MET_INSR + k_pMETpINSR_degradation)",
  k_INSR_expression = "k_pMETpINSR_degradation*pMETpINSR",
  k_MET_expression = "k_INSR_expression + 2*k_pMET_degradation*pMET",
  
  mTORC1 = "ERK*MET*k_activate_mTORC1_ERK*(INSR*k_dephospho_MET*k_phospho_ERK2*k_phospho_MET_INSR_basal + INSR*k_pMET_degradation*k_phospho_ERK2*k_phospho_MET_INSR_basal + MET*k_dephospho_MET_INSR*k_phospho_ERK*k_phospho_MET_basal + MET*k_pMETpINSR_degradation*k_phospho_ERK*k_phospho_MET_basal)/(k_deactivate_mTORC1*k_dephospho_pERK*(k_dephospho_MET*k_dephospho_MET_INSR + k_dephospho_MET*k_pMETpINSR_degradation + k_dephospho_MET_INSR*k_pMET_degradation + k_pMET_degradation*k_pMETpINSR_degradation))",
  pERK = "ERK*MET*(INSR*k_dephospho_MET*k_phospho_ERK2*k_phospho_MET_INSR_basal + INSR*k_pMET_degradation*k_phospho_ERK2*k_phospho_MET_INSR_basal + MET*k_dephospho_MET_INSR*k_phospho_ERK*k_phospho_MET_basal + MET*k_pMETpINSR_degradation*k_phospho_ERK*k_phospho_MET_basal)/(k_dephospho_pERK*(k_dephospho_MET*k_dephospho_MET_INSR + k_dephospho_MET*k_pMETpINSR_degradation + k_dephospho_MET_INSR*k_pMET_degradation + k_pMET_degradation*k_pMETpINSR_degradation))",
  pS6 = "ERK*MET*S6*k_activate_mTORC1_ERK*k_phospho_S6*(INSR*k_dephospho_MET*k_phospho_ERK2*k_phospho_MET_INSR_basal + INSR*k_pMET_degradation*k_phospho_ERK2*k_phospho_MET_INSR_basal + MET*k_dephospho_MET_INSR*k_phospho_ERK*k_phospho_MET_basal + MET*k_pMETpINSR_degradation*k_phospho_ERK*k_phospho_MET_basal)/(k_deactivate_mTORC1*k_dephospho_pERK*k_dephospho_pS6*(k_dephospho_MET*k_dephospho_MET_INSR + k_dephospho_MET*k_pMETpINSR_degradation + k_dephospho_MET_INSR*k_pMET_degradation + k_pMET_degradation*k_pMETpINSR_degradation))",
  pAKT = "AKT*MET*k_deactivate_mTORC1*k_dephospho_pERK*(INSR*k_dephospho_MET*k_phospho_AKT2*k_phospho_MET_INSR_basal + INSR*k_pMET_degradation*k_phospho_AKT2*k_phospho_MET_INSR_basal + MET*k_dephospho_MET_INSR*k_phospho_AKT*k_phospho_MET_basal + MET*k_pMETpINSR_degradation*k_phospho_AKT*k_phospho_MET_basal)/(k_dephospho_pAKT*(ERK*INSR*MET*k_activate_mTORC1_ERK*k_dephospho_MET*k_inhibit_AKT*k_phospho_ERK2*k_phospho_MET_INSR_basal + ERK*INSR*MET*k_activate_mTORC1_ERK*k_inhibit_AKT*k_pMET_degradation*k_phospho_ERK2*k_phospho_MET_INSR_basal + ERK*MET**2*k_activate_mTORC1_ERK*k_dephospho_MET_INSR*k_inhibit_AKT*k_phospho_ERK*k_phospho_MET_basal + ERK*MET**2*k_activate_mTORC1_ERK*k_inhibit_AKT*k_pMETpINSR_degradation*k_phospho_ERK*k_phospho_MET_basal + k_deactivate_mTORC1*k_dephospho_MET*k_dephospho_MET_INSR*k_dephospho_pERK + k_deactivate_mTORC1*k_dephospho_MET*k_dephospho_pERK*k_pMETpINSR_degradation + k_deactivate_mTORC1*k_dephospho_MET_INSR*k_dephospho_pERK*k_pMET_degradation + k_deactivate_mTORC1*k_dephospho_pERK*k_pMET_degradation*k_pMETpINSR_degradation))"
  
)


SS_equations <- resolveRecurrence(SS_vec)

# .. Export SS_equations -----
saveRDS(SS_equations, file.path(.dataFolder, "03-SS_equations_pMETpINSR.rds"))

# -------------------------------------------------------------------------#
# 2 Steady State equations model 1.5 ----
# -------------------------------------------------------------------------#

SS_vec <- c(
  
  pMETpMET = "MET**2*k_phospho_MET_basal/(k_dephospho_MET + k_pMET_degradation)",
  pMETpINSR = "INSR*MET*k_phospho_MET_INSR_basal/(k_dephospho_MET_INSR + k_pMETpINSR_degradation)",
  k_INSR_expression = "k_pMETpINSR_degradation*pMETpINSR",
  k_MET_expression = "k_INSR_expression + 2*k_pMET_degradation*pMETpMET",
  
  pS6K = "(ERK*k_phospho_S6K_ERK*(k_phospho_ERK2*pMETpINSR + k_phospho_ERK*pMETpMET)*S6K)/(k_dephospho_pERK*k_dephospho_S6K)",
  pERK = "(ERK*(k_phospho_ERK2*pMETpINSR + k_phospho_ERK*pMETpMET))/k_dephospho_pERK",
  pS6 = "(ERK*k_phospho_S6*k_phospho_S6K_ERK*(k_phospho_ERK2*pMETpINSR + k_phospho_ERK*pMETpMET)*S6*S6K)/(k_dephospho_pERK*k_dephospho_pS6*k_dephospho_S6K)",
  pAKT = "(AKT*k_dephospho_pERK*k_dephospho_S6K*(k_phospho_AKT2*pMETpINSR + k_phospho_AKT*pMETpMET))/(k_dephospho_pAKT*(k_dephospho_pERK*k_dephospho_S6K + ERK*k_inhibit_AKT*k_phospho_S6K_ERK*(k_phospho_ERK2*pMETpINSR + k_phospho_ERK*pMETpMET)*S6K))"
  
)

SS_equations <- resolveRecurrence(SS_vec)

# .. Export SS_equations -----
saveRDS(SS_equations, file.path(.dataFolder, "03-SS_equations_model1.5.rds"))

# -------------------------------------------------------------------------#
# 3 Steady State equations eMET ----
# -------------------------------------------------------------------------#

SS_vec <- c(
  
  eMET = "k_eMET_expression/k_eMET_degradation",
  pMET = "MET**2*k_phospho_MET_basal/k_dephospho_MET",
  pMETpINSR = "INSR*MET*k_phospho_MET_INSR_basal/k_dephospho_MET_INSR",
  eINSR = "k_eINSR_expression/k_eINSR_degradation",
  
  pERK = "ERK*MET*(INSR*k_dephospho_MET*k_phospho_ERK2*k_phospho_MET_INSR_basal + MET*k_dephospho_MET_INSR*k_phospho_ERK*k_phospho_MET_basal)/(k_dephospho_MET*k_dephospho_MET_INSR*k_dephospho_pERK)",
  S6K = "k_dephospho_pAKT*k_dephospho_pERK*k_dephospho_pS6K*pS6K*(k_dephospho_MET*k_inhibit_AKT*pS6K + k_dephospho_MET + k_inhibit_AKT*k_pMET_degradation*pS6K + k_pMET_degradation)/(MET*k_phospho_MET_basal*(AKT*k_dephospho_pERK*k_phospho_AKT*k_phospho_S6K_AKT + ERK*k_dephospho_pAKT*k_inhibit_AKT*k_phospho_ERK*k_phospho_S6K_ERK*pS6K + ERK*k_dephospho_pAKT*k_phospho_ERK*k_phospho_S6K_ERK))",
  pAKT = "AKT*MET*k_phospho_AKT*k_phospho_MET_basal/(k_dephospho_pAKT*(k_dephospho_MET*k_inhibit_AKT*pS6K + k_dephospho_MET + k_inhibit_AKT*k_pMET_degradation*pS6K + k_pMET_degradation))"
  
)


SS_equations <- resolveRecurrence(SS_vec)

# .. Export SS_equations -----
saveRDS(SS_equations, file.path(.dataFolder, "03-SS_equations_eMET.rds"))


# -------------------------------------------------------------------------#
# 4 Steady State equations IRS1 (negative SS possible :( ----
# -------------------------------------------------------------------------#

SS_vec <- c(
  
  pMETpMET = "MET**2*k_phospho_MET_basal/(k_dephospho_MET + k_pMET_degradation)",
  pMETpINSR = "INSR*MET*k_phospho_MET_INSR_basal/(k_dephospho_MET_INSR + k_pMETpINSR_degradation)",
  k_INSR_expression = "k_pMETpINSR_degradation*pMETpINSR",
  k_MET_expression = "k_INSR_expression + 2*k_pMET_degradation*pMETpMET",
  
  pERK = "ERK*(k_dephospho_IRS1_Y*(k_phospho_ERK_INSR*pMETpINSR + k_phospho_ERK_MET*pMETpMET) + k_phospho_ERK_IRS1*(IRS1*k_phospho_IRS1_INSR*pMETpINSR + IRS1*k_phospho_IRS1_MET*pMETpMET - IRS1_YS*k_degrade_IRS1))/(k_dephospho_IRS1_Y*k_dephospho_pERK)",
  pS6 = "ERK*S6*S6K*k_phospho_S6*k_phospho_S6K_ERK*(k_dephospho_IRS1_Y*(k_phospho_ERK_INSR*pMETpINSR + k_phospho_ERK_MET*pMETpMET) + k_phospho_ERK_IRS1*(IRS1*k_phospho_IRS1_INSR*pMETpINSR + IRS1*k_phospho_IRS1_MET*pMETpMET - IRS1_YS*k_degrade_IRS1))/(k_dephospho_IRS1_Y*k_dephospho_S6K*k_dephospho_pERK*k_dephospho_pS6)",
  pAKT = "AKT*k_dephospho_S6K*k_dephospho_pERK*(IRS1*k_phospho_AKT_IRS1*k_phospho_IRS1_INSR*pMETpINSR + IRS1*k_phospho_AKT_IRS1*k_phospho_IRS1_MET*pMETpMET - IRS1_YS*k_degrade_IRS1*k_phospho_AKT_IRS1 + k_dephospho_IRS1_Y*k_phospho_AKT_INSR*pMETpINSR + k_dephospho_IRS1_Y*k_phospho_AKT_MET*pMETpMET)/(k_dephospho_pAKT*(ERK*IRS1*S6K*k_inhibit_AKT*k_phospho_ERK_IRS1*k_phospho_IRS1_INSR*k_phospho_S6K_ERK*pMETpINSR + ERK*IRS1*S6K*k_inhibit_AKT*k_phospho_ERK_IRS1*k_phospho_IRS1_MET*k_phospho_S6K_ERK*pMETpMET - ERK*IRS1_YS*S6K*k_degrade_IRS1*k_inhibit_AKT*k_phospho_ERK_IRS1*k_phospho_S6K_ERK + ERK*S6K*k_dephospho_IRS1_Y*k_inhibit_AKT*k_phospho_ERK_INSR*k_phospho_S6K_ERK*pMETpINSR + ERK*S6K*k_dephospho_IRS1_Y*k_inhibit_AKT*k_phospho_ERK_MET*k_phospho_S6K_ERK*pMETpMET + k_dephospho_IRS1_Y*k_dephospho_S6K*k_dephospho_pERK))",
  pS6K = "ERK*S6K*k_phospho_S6K_ERK*(k_dephospho_IRS1_Y*(k_phospho_ERK_INSR*pMETpINSR + k_phospho_ERK_MET*pMETpMET) + k_phospho_ERK_IRS1*(IRS1*k_phospho_IRS1_INSR*pMETpINSR + IRS1*k_phospho_IRS1_MET*pMETpMET - IRS1_YS*k_degrade_IRS1))/(k_dephospho_IRS1_Y*k_dephospho_S6K*k_dephospho_pERK)",
  IRS1_Y = "(IRS1*k_phospho_IRS1_INSR*pMETpINSR + IRS1*k_phospho_IRS1_MET*pMETpMET - IRS1_YS*k_degrade_IRS1)/k_dephospho_IRS1_Y",
  k_phospho_IRS1_S6K = "IRS1_YS*k_degrade_IRS1/(IRS1_Y*pS6K) + IRS1_YS*k_dephospho_IRS1_YS/(IRS1_Y*pS6K)",
  k_IRS1_expression = "IRS1_YS*k_degrade_IRS1"
)


SS_equations <- resolveRecurrence(SS_vec)

# -------------------------------------------------------------------------#
# 4 Steady State equations IRS1 ----
# -------------------------------------------------------------------------#

SS_vec <- c(
  
  pMETpMET = "MET**2*k_phospho_MET_basal/(k_dephospho_MET + k_pMET_degradation)",
  pMETpINSR = "INSR*MET*k_phospho_MET_INSR_basal/(k_dephospho_MET_INSR + k_pMETpINSR_degradation)",
  k_INSR_expression = "k_pMETpINSR_degradation*pMETpINSR",
  k_MET_expression = "k_INSR_expression + 2*k_pMET_degradation*pMETpMET",
  
  IRS1 = "(k_degrade_IRS1*k_dephospho_IRS1_Y*k_IRS1_expression + k_dephospho_IRS1_Y*k_dephospho_IRS1_YS*k_IRS1_expression + k_degrade_IRS1*k_IRS1_expression*k_phospho_IRS1_S6K*pS6K)/(k_degrade_IRS1*k_phospho_IRS1_INSR*k_phospho_IRS1_S6K*pMETpINSR*pS6K + k_degrade_IRS1*k_phospho_IRS1_MET*k_phospho_IRS1_S6K*pMETpMET*pS6K)",
  IRS1_Y = "((k_degrade_IRS1 + k_dephospho_IRS1_YS)*k_IRS1_expression)/(k_degrade_IRS1*k_phospho_IRS1_S6K*pS6K)",
  IRS1_YS = "k_IRS1_expression/k_degrade_IRS1",
  pERK = "(ERK*(k_degrade_IRS1*k_IRS1_expression*k_phospho_ERK_IRS1 + k_dephospho_IRS1_YS*k_IRS1_expression*k_phospho_ERK_IRS1 + k_degrade_IRS1*k_phospho_IRS1_S6K*(k_phospho_ERK_INSR*pMETpINSR + k_phospho_ERK_MET*pMETpMET)*pS6K))/(k_degrade_IRS1*k_dephospho_pERK*k_phospho_IRS1_S6K*pS6K)",
  pAKT = "(AKT*k_degrade_IRS1*k_IRS1_expression*k_phospho_AKT_IRS1 + AKT*k_dephospho_IRS1_YS*k_IRS1_expression*k_phospho_AKT_IRS1 + AKT*k_degrade_IRS1*k_phospho_AKT_INSR*k_phospho_IRS1_S6K*pMETpINSR*pS6K + AKT*k_degrade_IRS1*k_phospho_AKT_MET*k_phospho_IRS1_S6K*pMETpMET*pS6K)/(k_degrade_IRS1*k_dephospho_pAKT*k_phospho_IRS1_S6K*pS6K + k_degrade_IRS1*k_dephospho_pAKT*k_inhibit_AKT*k_phospho_IRS1_S6K*pS6K^2)",
  S6K = "(k_degrade_IRS1*k_dephospho_pERK*k_dephospho_S6K*k_phospho_IRS1_S6K*pS6K^2)/(ERK*k_phospho_S6K_ERK*(k_degrade_IRS1*k_IRS1_expression*k_phospho_ERK_IRS1 + k_dephospho_IRS1_YS*k_IRS1_expression*k_phospho_ERK_IRS1 + k_degrade_IRS1*k_phospho_IRS1_S6K*(k_phospho_ERK_INSR*pMETpINSR + k_phospho_ERK_MET*pMETpMET)*pS6K))",
  pS6 = "(k_phospho_S6*pS6K*S6)/k_dephospho_pS6"
)

SS_equations <- resolveRecurrence(SS_vec)

# .. Export SS_equations -----
saveRDS(SS_equations, file.path(.dataFolder, "03-SS_equations_IRS1.rds"))

# Exit ----






