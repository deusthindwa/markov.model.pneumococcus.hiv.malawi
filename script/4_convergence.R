#Written by Deus Thindwa
#Estimating the contribution of HIV-infected adults to household pneumococcal transmission in South Africa, 2016-2018.
#Continuous-time time-homogeneous hidden Markov modelling study, PhD chapter 1.
#20/9/2019 - 10/6/2020 

#run multiple chains to assess convergence of the selected model
j = 0.001; k = 0.038; l = 0.01; m = 0.48
for(i in 1:5){
  
  sink("/Users/lusako/Documents/PneumoDuDe data/convergence.txt",append=TRUE)
  p.convrg <- msm(state ~ dys, subject = pid, data = pneumodel,
                  qmatrix=rbind(c(0.0, j, l), 
                                c(k, 0.0, 0.0),
                                c(m, 0.0, 0.0)),
                  covariates = list("1-2" =~ hiv + agegp + sex + nochild + season, "2-1" =~ hiv + agegp + sex + season + abx,
                                    "1-3" =~ hiv + agegp + sex + nochild + season, "3-1" =~ hiv + agegp + sex + season + abx),
                  control = list(fnscale=1000, maxit = 100000, trace=1, REPORT=1))
  sink()
  j = j+0.015; k = k-0.009; l = l+0.03; m = m-0.008
}

#model convergence
pneumo.cg <- import(here("convergence.xlsx"))

pneumo.cg <- pneumo.cg %>%
  select(chain,iter,lik) %>%
  mutate(chaincat = if_else(chain == 1, "1 (q12=0.001, q21=0.038) >> (q12=0.049, q21=0.017)",
                            if_else(chain == 2,"2 (q12=0.016, q21=0.029) >> (q12=0.049, q21=0.017)",
                                    if_else(chain == 3,"3 (q12=0.031, q21=0.002) >> (q12=0.049, q21=0.017)",
                                            if_else(chain == 4,"4 (q12=0.046, q21=0.011) >> (qq12=0.049, q21=0.017)",
                                                    if_else(chain == 5, "5 (q12=0.061, q21=0.002) >> (q12=0.049, q21=0.017)",
                                                            if_else(chain == 6,"1 (q13=0.010, q31=0.048) >> (q13=0.116, q31=0.022)",
                                                                    if_else(chain == 7,"2 (q13=0.040, q31=0.040) >> (q13=0.116, q31=0.022)",
                                                                            if_else(chain == 8,"3 (q13=0.070, q31=0.032) >> (q13=0.116, q31=0.022)",
                                                                                    if_else(chain == 9,"4 (q13=0.100, q31=0.024) >> (q13=0.116, q31=0.022)",
                                                                                            "5 (q13=0.130, q31=0.016) >> (q13=0.116, q31=0.022)"))))))))),
         infectstatus = if_else(chain <=5, "VT transition intensity steady states", "NVT transition intensity steady states")
         )

A <- ggplot(filter(pneumo.cg, infectstatus == "VT transition intensity steady states"), aes(iter,lik,color=chaincat)) + 
  geom_line(size=0.8) + 
  labs(title="A", x="Iteration",y="-2Log-likelihood") + 
  xlim(0,1000) + #scale_x_break(c(400, 600), scales = 200) + scale_x_break(c(800, 900), scales = 100) +
  #scale_y_continuous(breaks=c(1500, 2000, 2500,3000,3500,4000,4500,5000)) + 
  coord_cartesian(xlim = c(0,50)) +
  theme_bw() + 
  theme(axis.text.x=element_text(face="bold",size=10),axis.text.y=element_text(face="bold",size=10)) +
  theme(legend.position=c(0.55,0.6),legend.key.height=unit(0.8,"line"),legend.key.width=unit(1,"line")) + 
  guides(color=guide_legend(title="Chain (initial intensity) >> (final baseline intensity)")) 

B <- ggplot(filter(pneumo.cg, infectstatus == "NVT transition intensity steady states"), aes(iter,lik,color=chaincat)) + 
  geom_line(size=0.8) + 
  labs(title="B", x="Iteration",y="-2Log-likelihood") + 
  xlim(0,1000) +
  #scale_y_continuous(breaks=c(1,2,4,5)) + 
  coord_cartesian(xlim = c(0,50)) +
  theme_bw() + 
  theme(axis.text.x=element_text(face="bold",size=10),axis.text.y=element_text(face="bold",size=10)) +
  theme(legend.position=c(0.55,0.6),legend.key.height=unit(0.8,"line"),legend.key.width=unit(1,"line")) + 
  guides(color=guide_legend(title="Chain (initial intensity) >> (final baseline intensity)")) 

A|B

#observed versus predicted prevalence of selected model
p.obsexp <- msm(state ~ dys, subject = pid, data = pneumodel,
                qmatrix = matrix.Q,
                covariates = list("1-2" = ~ hiv + agegp + sex + nochild + season + ses, "2-1" =~ hiv + agegp + sex + season + abx + dens, 
                                  "1-3" = ~ hiv + agegp + sex + nochild + season + ses, "3-1" =~ hiv + agegp + sex + season + abx + dens),
                opt.method = "bobyqa", control = list(maxfun = 1000000))

pneumo.oe <- tk_tbl(prevalence.msm(p.obsexp, times = seq(0,338,14)), preserve_index = TRUE, rename_index = "Time")

pneumo.oe <- rename(pneumo.oe, c("obs.clear" = "Observed.State.1", "VT.obs.carry" = "Observed.State.2", "NVT.obs.carry" = "Observed.State.3", "obs.p.clear" = "Observed.percentages.State.1",
                                 "VT.obs.p.carry" = "Observed.percentages.State.2", "NVT.obs.p.carry" = "Observed.percentages.State.3", "exp.clear" = "Expected.Clear",
                                 "VT.exp.carry" = "Expected.VT_carry", "NVT.exp.carry" = "Expected.NVT_carry", "exp.p.clear" = "Expected.percentages.Clear", "VT.exp.p.carry" = "Expected.percentages.VT_carry",
                                 "NVT.exp.p.carry" = "Expected.percentages.NVT_carry"))

pneumo.oe$lci.clear=pneumo.oe$exp.p.clear/100-(1.96*sqrt(pneumo.oe$exp.p.clear/100*(1-pneumo.oe$exp.p.clear/100)/pneumo.oe$exp.clear)) 
pneumo.oe$uci.clear=pneumo.oe$exp.p.clear/100+(1.96*sqrt(pneumo.oe$exp.p.clear/100*(1-pneumo.oe$exp.p.clear/100)/pneumo.oe$exp.clear))
pneumo.oe$VT.lci.carry=pneumo.oe$VT.exp.p.carry/100-(1.96*sqrt(pneumo.oe$VT.exp.p.carry/100*(1-pneumo.oe$VT.exp.p.carry/100)/pneumo.oe$VT.exp.carry)) 
pneumo.oe$VT.uci.carry=pneumo.oe$VT.exp.p.carry/100+(1.96*sqrt(pneumo.oe$VT.exp.p.carry/100*(1-pneumo.oe$VT.exp.p.carry/100)/pneumo.oe$VT.exp.carry))
pneumo.oe$NVT.lci.carry=pneumo.oe$NVT.exp.p.carry/100-(1.96*sqrt(pneumo.oe$NVT.exp.p.carry/100*(1-pneumo.oe$NVT.exp.p.carry/100)/pneumo.oe$NVT.exp.carry)) 
pneumo.oe$NVT.uci.carry=pneumo.oe$NVT.exp.p.carry/100+(1.96*sqrt(pneumo.oe$NVT.exp.p.carry/100*(1-pneumo.oe$NVT.exp.p.carry/100)/pneumo.oe$NVT.exp.carry))

#plot observed and predicted carriage
cols <- c("Observed vs predicted clearance"="#0000FF","Observed vs predicted carriage"="#FF0000")
C <- ggplot(pneumo.oe,aes(Time)) + 
  geom_point(aes(Time,obs.p.clear,color="Observed vs predicted clearance"),size=2,shape=5) + 
  geom_line(aes(Time,exp.p.clear,color="Observed vs predicted clearance"),size=1) + 
  geom_ribbon(aes(ymin=lci.clear*100,ymax=uci.clear*100, color="Observed vs predicted clearance"),alpha=0.2,size=0.1) +
  geom_point(aes(Time,VT.obs.p.carry,color="Observed vs predicted carriage"),size=2,shape=5) + 
  geom_line(aes(Time,VT.exp.p.carry,color="Observed vs predicted carriage"),size=1) + 
  geom_ribbon(aes(ymin=VT.lci.carry*100,ymax=VT.uci.carry*100, color="Observed vs predicted carriage"),alpha=0.2,size=0.1) +
  labs(title="C", x="Days",y="Prevalence (%)") + 
  scale_x_continuous(breaks=c(0,40,80,120,160,200,240,280)) + 
  scale_y_continuous(breaks=c(10,30,40,50,60),) + 
  theme_bw() + 
  theme(axis.text.x=element_text(face="bold",size=10), axis.text.y=element_text(face="bold",size=10)) +
  theme(legend.position=c(0.25,0.85)) + 
  guides(color=guide_legend(title="")) 

D <- ggplot(pneumo.oe,aes(Time)) + 
  geom_point(aes(Time,obs.p.clear,color="Observed vs predicted clearance"),size=2,shape=5) + 
  geom_line(aes(Time,exp.p.clear,color="Observed vs predicted clearance"),size=1) + 
  geom_ribbon(aes(ymin=lci.clear*100,ymax=uci.clear*100, color="Observed vs predicted clearance"),alpha=0.2,size=0.1) +
  geom_point(aes(Time,NVT.obs.p.carry,color="Observed vs predicted carriage"),size=2,shape=5) + 
  geom_line(aes(Time,NVT.exp.p.carry,color="Observed vs predicted carriage"),size=1) + 
  geom_ribbon(aes(ymin=NVT.lci.carry*100,ymax=NVT.uci.carry*100, color="Observed vs predicted carriage"),alpha=0.2,size=0.1) +
  labs(title="D", x="Days",y="Prevalence (%)") + 
  scale_x_continuous(breaks=c(0,40,80,120,160,200,240,280)) + 
  scale_y_continuous(breaks=c(10,30,40,50,60)) + 
  theme_bw() + 
  theme(axis.text.x=element_text(face="bold",size=10), axis.text.y=element_text(face="bold",size=10)) +
  theme(legend.position=c(0.25,0.90)) + 
  guides(color=guide_legend(title="")) 

C|D
#-------------------------------------------------------
#plot observed versus predicted carriage
pneumo.pcla <- subset(pneumo.oe,select=c(obs.clear));pneumo.pcla$state1<-"Observed clearance"
pneumo.pcla <- rename(pneumo.pcla, c("obs.clear" = "value1"))
pneumo.pclb <- subset(pneumo.oe,select=c(VT.exp.clear));pneumo.pclb$state2<-"Predicted clearance"
pneumo.pclb <- rename(pneumo.pclb, c("exp.clear" = "value2"))
pneumo.pcl1 <-cbind(pneumo.pcla,pneumo.pclb)
colx <- c(pneumo.pcl1$state1,pneumo.pcl1$state2)

pneumo.pclc <- subset(pneumo.oe,select=c(obs.carry));pneumo.pclc$state1<-"Observed carriage"
pneumo.pclc <- rename(pneumo.pclc, c("obs.carry" = "value1"))
pneumo.pcld <- subset(pneumo.oe,select=c(exp.carry));pneumo.pcld$state2<-"Predicted carriage"
pneumo.pcld <- rename(pneumo.pcld, c("exp.carry" = "value2"))
pneumo.pcl2 <-cbind(pneumo.pclc,pneumo.pcld)

ggplot() + 
  geom_point(aes(pneumo.pcl1$value1,pneumo.pcl1$value2),color=,size=3) + 
  geom_point(aes(pneumo.pcl2$value1,pneumo.pcl2$value2),size=3) + 
  geom_abline(intercept=0,slope=1,linetype=2) + 
  labs(title="C", x="Observed",y="Predicted") + 
  theme_bw() + 
  ylim(350,1150) + 
  xlim(350,1150) + 
  theme(axis.text.x=element_text(face="bold",size=10), axis.text.y=element_text(face="bold",size=10)) +
  theme(legend.position=c(0.2,0.8)) + 
  guides(color=guide_legend(title=""),shape=guide_legend(title="")) 

A + B + C