#Written by Deus Thindwa
#Estimating the contribution of HIV-infected adults to household pneumococcal transmission in South Africa, 2016-2018.
#Continuous-time time-homogeneous hidden Markov modelling study, PhD chapter 1.
#20/9/2019 - 10/6/2020

#hazard ratios for pneumococcal acquisition/clearance rates (Table 2)
hazard.msm(p.model5,hazard.scale=1,cl=0.95)

#-------------------------------------------------------
#acquisition probability
p.modela <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV-", co),ci="normal",cl=0.95)
p.modelb <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV+ART+"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART"))

pneumo.es0$carry.est <- c(p.modela$estimates[1,2],p.modelb$estimates[1,2])
pneumo.es0$Lcarry.est <- c(p.modela$L[1,2],p.modelb$L[1,2])
pneumo.es0$Ucarry.est <- c(p.modela$U[1,2],p.modelb$U[1,2])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART"))

pneumo.es1$carry.est <- c(p.modela$estimates[1,3],p.modelb$estimates[1,3])
pneumo.es1$Lcarry.est <- c(p.modela$L[1,3],p.modelb$L[1,3])
pneumo.es1$Ucarry.est <- c(p.modela$U[1,3],p.modelb$U[1,3])

pneumo <- rbind(pneumo.es0, pneumo.es1)

A <-
  ggplot(pneumo) +
  geom_errorbar(aes(hivst,color=hivst,ymin=Lcarry.est,ymax=Ucarry.est),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,carry.est,color=hivst),shape=8,size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  ylim(0.01,0.08) +
  labs(title="A",x="",y="acquisition probability") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  guides(color=guide_legend(title=""),fill=FALSE) +
  theme(legend.text=element_text(size=10),legend.position="none",legend.title=element_text(face="bold",size=10))
#format and present community acquisition probabilities (supplementary Table 2)
pneumo.es <- rename(pneumo.es, c("iid"="Table.1B","age"="Age","hiv"="HIV","carry.est"="Estimate","Lcarry.est"="Lower.bound","Ucarry.est"="Upper.bound"))

#-------------------------------------------------------
#acquisition probability
p.modela <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV-", nochild = "1 child"),ci="normal",cl=0.95)
p.modelb <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV+ART+", nochild = "1 child"),ci="normal",cl=0.95)
p.modelc <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV-", nochild = "2+ children"),ci="normal",cl=0.95)
p.modeld <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV+ART+", nochild = "2+ children"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART+","VT, HIV-","VT, HIV+ART+"),
                         "nochild" = c("1 child", "1 child", "2+ children","2+ children"))

pneumo.es0$carry.est <- c(p.modela$estimates[2,1], p.modelb$estimates[2,1], p.modelc$estimates[2,1], p.modeld$estimates[2,1])
pneumo.es0$Lcarry.est <- c(p.modela$L[2,1], p.modelb$L[2,1], p.modelc$L[2,1], p.modeld$L[2,1])
pneumo.es0$Ucarry.est <- c(p.modela$U[2,1], p.modelb$U[2,1], p.modelc$U[2,1], p.modeld$U[2,1])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART+","NVT, HIV-","NVT, HIV+ART+"),
                         "nochild" = c("1 child", "1 child", "2+ children","2+ children"))

pneumo.es1$carry.est <- c(p.modela$estimates[3,1], p.modelb$estimates[3,1], p.modelc$estimates[3,1], p.modeld$estimates[3,1])
pneumo.es1$Lcarry.est <- c(p.modela$L[3,1], p.modelb$L[3,1], p.modelc$L[3,1], p.modeld$L[3,1])
pneumo.es1$Ucarry.est <- c(p.modela$U[3,1], p.modelb$U[3,1], p.modelc$U[3,1], p.modeld$U[3,1])

pneumo <- rbind(pneumo.es0, pneumo.es1)

B <-
  ggplot(pneumo) + 
  geom_errorbar(aes(hivst,color=hivst,ymin=1/Lcarry.est,ymax=1/Ucarry.est,shape=nochild),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,1/carry.est,color=hivst,shape=nochild),size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  labs(title="B",x="",y="Average carriage duration by abx (days)") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  guides(color=guide_legend(title="Serotype, HIV status"),shape=guide_legend(title="On ABX"),fill=FALSE) +
  theme(legend.text=element_text(size=10),legend.position="right",legend.title=element_text(face="bold",size=10))

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




