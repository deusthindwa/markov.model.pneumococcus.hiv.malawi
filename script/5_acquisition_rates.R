#Written by Deus Thindwa
#Estimating the contribution of HIV-infected adults to household pneumococcal transmission in South Africa, 2016-2018.
#Continuous-time time-homogeneous hidden Markov modelling study, PhD chapter 1.
#20/9/2019 - 10/6/2020

#hazard ratios for pneumococcal acquisition/clearance rates (Table 1)
hazard.msm(spn_modelfit, hazard.scale = 1, cl = 0.95)

#====================================================================

#acquisition probability
p.modela <- pmatrix.msm(spn_modelfit, t = 1, covariates = list(hiv = "HIV-"), ci = "bootstrap", cl = 0.95)
p.modelb <- pmatrix.msm(spn_modelfit, t = 1, covariates = list(hiv = "HIV+ART+"), ci = "boostrap" , cl = 0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART"))
pneumo.es0$carry.est <- c(p.modela$estimates[1,2], p.modelb$estimates[1,2])
pneumo.es0$Lcarry.est <- c(p.modela$L[1,2], p.modelb$L[1,2])
pneumo.es0$Ucarry.est <- c(p.modela$U[1,2], p.modelb$U[1,2])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART"))
pneumo.es1$carry.est <- c(p.modela$estimates[1,3], p.modelb$estimates[1,3])
pneumo.es1$Lcarry.est <- c(p.modela$L[1,3], p.modelb$L[1,3])
pneumo.es1$Ucarry.est <- c(p.modela$U[1,3], p.modelb$U[1,3])

pneumo <- rbind(pneumo.es0, pneumo.es1)
  
A <-
  ggplot(pneumo) +
  geom_errorbar(aes(hivst, color = hivst, ymin = Lcarry.est, ymax = Ucarry.est), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_point(aes(hivst, carry.est, color = hivst), shape = 11, size = 3, position = position_dodge(width = 0.5), stat = "identity") +
  theme_bw() + 
  ylim(0,0.2) +
  labs(title = "A", x = "", y = "Carriage acquisition probability") + 
  theme(axis.text.y = element_text(face = "bold", size = 10)) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  guides(color = guide_legend(title = ""), fill = FALSE) +
  theme(legend.text = element_text(size = 10), legend.position = "none", legend.title = element_text(face = "bold", size = 10))

#format and present community acquisition probabilities (supplementary Table 2)
pneumo.es <- rename(pneumo.es, c("iid"="Table.1B","age"="Age","hiv"="HIV","carry.est"="Estimate","Lcarry.est"="Lower.bound","Ucarry.est"="Upper.bound"))

#====================================================================

#acquisition probability
p.modela <- pmatrix.msm(spn_modelfit, t = 1, covariates = list(hiv = "HIV-", sex = "Male"), ci = "normal", cl = 0.95)
p.modelb <- pmatrix.msm(spn_modelfit, t = 1, covariates = list(hiv = "HIV+ART+", sex = "Male"), ci = "normal",cl = 0.95)
p.modelc <- pmatrix.msm(spn_modelfit, t = 1, covariates = list(hiv = "HIV-", sex = "Female"),ci = "normal",cl = 0.95)
p.modeld <- pmatrix.msm(spn_modelfit, t = 1, covariates = list(hiv = "HIV+ART+", sex = "Female"),ci = "normal",cl = 0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART+","VT, HIV-","VT, HIV+ART+"),
                         "sex" = c("Male", "Male", "Female","Female"))

pneumo.es0$carry.est <- c(p.modela$estimates[1,2], p.modelb$estimates[1,2], p.modelc$estimates[1,2], p.modeld$estimates[1,2])
pneumo.es0$Lcarry.est <- c(p.modela$L[1,2], p.modelb$L[1,2], p.modelc$L[1,2], p.modeld$L[1,2])
pneumo.es0$Ucarry.est <- c(p.modela$U[1,2], p.modelb$U[1,2], p.modelc$U[1,2], p.modeld$U[1,2])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART+","NVT, HIV-","NVT, HIV+ART+"),
                         "sex" = c("Male", "Male", "Female","Female"))

pneumo.es1$carry.est <- c(p.modela$estimates[1,3], p.modelb$estimates[1,3], p.modelc$estimates[3,1], p.modeld$estimates[3,1])
pneumo.es1$Lcarry.est <- c(p.modela$L[1,3], p.modelb$L[1,3], p.modelc$L[1,3], p.modeld$L[1,3])
pneumo.es1$Ucarry.est <- c(p.modela$U[1,3], p.modelb$U[1,3], p.modelc$U[1,3], p.modeld$U[1,3])

pneumo <- rbind(pneumo.es0, pneumo.es1)

B <-
  ggplot(pneumo) +
  geom_errorbar(aes(hivst, color = hivst, group = sex, ymin = Lcarry.est, ymax = Ucarry.est), width = 0, size = 1, position = position_dodge(width = 0.5)) +
  geom_point(aes(hivst, carry.est, color = hivst, group = sex), shape = 11, size = 3, position = position_dodge(width = 0.5), stat = "identity") +
  theme_bw() + 
  ylim(0,0.2) +
  labs(title = "A", x = "", y = "Carriage acquisition probability") + 
  theme(axis.text.y = element_text(face = "bold", size = 10)) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  guides(color = guide_legend(title = ""), fill = FALSE) +
  theme(legend.text = element_text(size = 10), legend.position = "none", legend.title = element_text(face = "bold", size = 10))

#-------------------------------------------------------
#estimate the number of episodes
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Younger child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Younger child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Older child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Older child",tx="hhtx",ahivcat="No"),ci="normal",cl=0.95)

envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Younger child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Younger child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Pos",agecat="Older child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)
envisits.msm(p.model5,fromt=0,tot=365.25,covariates=list(hiv="Neg",agecat="Older child",tx="hhtx",ahivcat="Yes"),ci="normal",cl=0.95)




