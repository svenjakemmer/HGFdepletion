# -------------------------------------------------------------------------#
# 0 Steady States ----
# -------------------------------------------------------------------------#
#
# S003-SteadyStateCalculation.R
#
# [PURPOSE]
# 
# Calculate Steady State with Mathematica
#
# [AUTHOR]
# Svenja
#
# [Date]
# 2020-07-07
#

# -------------------------------------------------------------------------#
# 1 Calculate Steady State with Mathematica ----
# -------------------------------------------------------------------------#

# .. 1 Get Output for Mathematica -----
reactions_SS %>% as.eqnvec() %>% str_subset_name(c("S6", "AKT")) %>% print_mathematica.eqnvec()
reactions_SS %>% as.eqnvec() %>% str_subset_name(c("S6", "AKT")) %>% names %>% print_mathematica.character() %>% .[[1]]

myreactions_downstream %>% as.eqnvec() %>% print_mathematica.eqnvec()
myreactions_downstream %>% as.eqnvec() %>% names %>% print_mathematica.character() %>% .[[1]]

# .. 2 To solve in Mathematica -----

# mysolution = Solve[f==0, x];
# Export["mySS.txt", mysolution//Simplify]
# gives "solutions"

# .. 3 Retransform Mathematica Output -----

solutions <- structure(c("(kdegradeIRS1*kdephosphoIRS1Y*kIRS1expression + kdephosphoIRS1Y*kdephosphoIRS1YS*kIRS1expression + kdegradeIRS1*kIRS1expression*kphosphoIRS1S6K*pS6K)/(kdegradeIRS1*kphosphoIRS1INSR*kphosphoIRS1S6K*pMETpINSR*pS6K + kdegradeIRS1*kphosphoIRS1MET*kphosphoIRS1S6K*pMETpMET*pS6K)",
                         "((kdegradeIRS1 + kdephosphoIRS1YS)*kIRS1expression)/(kdegradeIRS1*kphosphoIRS1S6K*pS6K)",
                         "kIRS1expression/kdegradeIRS1",
                         "(ERK*(kdegradeIRS1*kIRS1expression*kphosphoERKIRS1 + kdephosphoIRS1YS*kIRS1expression*kphosphoERKIRS1 + kdegradeIRS1*kphosphoIRS1S6K*(kphosphoERKINSR*pMETpINSR + kphosphoERKMET*pMETpMET)*pS6K))/(kdegradeIRS1*kdephosphopERK*kphosphoIRS1S6K*pS6K)",
                         "(AKT*kdegradeIRS1*kIRS1expression*kphosphoAKTIRS1 + AKT*kdephosphoIRS1YS*kIRS1expression*kphosphoAKTIRS1 + AKT*kdegradeIRS1*kphosphoAKTINSR*kphosphoIRS1S6K*pMETpINSR*pS6K + AKT*kdegradeIRS1*kphosphoAKTMET*kphosphoIRS1S6K*pMETpMET*pS6K)/(kdegradeIRS1*kdephosphopAKT*kphosphoIRS1S6K*pS6K + kdegradeIRS1*kdephosphopAKT*kinhibitAKT*kphosphoIRS1S6K*pS6K^2)",
                         "(kdegradeIRS1*kdephosphopERK*kdephosphoS6K*kphosphoIRS1S6K*pS6K^2)/(ERK*kphosphoS6KERK*(kdegradeIRS1*kIRS1expression*kphosphoERKIRS1 + kdephosphoIRS1YS*kIRS1expression*kphosphoERKIRS1 + kdegradeIRS1*kphosphoIRS1S6K*(kphosphoERKINSR*pMETpINSR + kphosphoERKMET*pMETpMET)*pS6K))",
                         "(kphosphoS6*pS6K*S6)/kdephosphopS6"
), .Names = c("IRS1", "IRS1Y","IRS1YS", "pERK","pAKT", "S6K", "pS6"))

# resubstitute original characters
replacements <- reactions_SS %>% as.eqnvec()  %>% getSymbols() %>% print_mathematica.character() %>% .[[2]]
print(solutions %>% str_replace_all(replacements))




