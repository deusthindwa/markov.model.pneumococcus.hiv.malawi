

#-------------------------------------------------------
#estimate the number of episodes
envisits.msm(spn_modelfit, fromt = 0, tot = 365.25, ci = "normal", cl = 0.95)
envisits.msm(spn_modelfit, fromt = 0, tot = 365.25, covariates = list(hiv = "HIV+ART+"), ci = "normal", cl = 0.95)
envisits.msm(spn_modelfit, fromt = 0, tot = 365.25, covariates = list(hiv = "HIV-"), ci = "normal", cl = 0.95)

#Probability that each state is next
pnext.msm(spn_modelfit, ci = "normal", cl = 0.95)
pnext.msm(spn_modelfit, covariates = list(hiv = "HIV+ART+"), ci = "normal", cl = 0.95)
pnext.msm(spn_modelfit, covariates = list(hiv = "HIV-"), ci = "normal", cl = 0.95)

#For processes with successive periods of recovery and relapse, we may want to forecast the total time spent healthy or diseased, before death.
totlos.msm(spn_modelfit, fromt = 0, tot = 365.25, ci = "normal", cl = 0.95)
totlos.msm(spn_modelfit, fromt = 0, tot = 365.25, covariates = list(hiv = "HIV+ART+"), ci = "normal", cl = 0.95)
totlos.msm(spn_modelfit, fromt = 0, tot = 365.25, covariates = list(hiv = "HIV-"), ci = "normal", cl = 0.95)

#Ratio of transition intensities
qratio.msm(cav.msm, ind1=c(2,1), ind2=c(1,2))

#hazard ratios for pneumococcal acquisition/clearance rates (Table 1)
hazard.msm(spn_modelfit, hazard.scale = 1, cl = 0.95)


