#Written by Deus Thindwa
#Estimating the contribution of HIV-infected adults to household pneumococcal transmission in South Africa, 2016-2018.
#Continuous-time time-homogeneous hidden Markov modelling study, PhD chapter 1.
#20/9/2019 - 10/6/2020

#average carriage duration by ABX status
p.modela <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-"),ci="normal",cl=0.95)
p.modelb <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART"))

pneumo.es0$clear.est <- c(p.modela$estimates[2,1], p.modelb$estimates[2,1])
pneumo.es0$Lclear.est <- c(p.modela$L[2,1], p.modelb$L[2,1])
pneumo.es0$Uclear.est <- c(p.modela$U[2,1], p.modelb$U[2,1])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART"))

pneumo.es1$clear.est <- c(p.modela$estimates[3,1], p.modelb$estimates[3,1])
pneumo.es1$Lclear.est <- c(p.modela$L[3,1], p.modelb$L[3,1])
pneumo.es1$Uclear.est <- c(p.modela$U[3,1], p.modelb$U[3,1])

pneumo <- rbind(pneumo.es0, pneumo.es1)

A <- 
  ggplot(pneumo) + 
  geom_errorbar(aes(hivst,color=hivst,ymin=1/Lclear.est,ymax=1/Uclear.est),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,1/clear.est,color=hivst),shape=8,size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  labs(title="A",x="",y="Overall average carriage duration (days)") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  theme(legend.position="none")
#-------------------------------------------------------

p.modela <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", abx="taken"),ci="normal",cl=0.95)
p.modelb <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", abx="taken"),ci="normal",cl=0.95)
p.modelc <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", abx="nottaken"),ci="normal",cl=0.95)
p.modeld <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", abx="nottaken"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART+","VT, HIV-","VT, HIV+ART+"),
                         "abx" = c("taken", "taken", "nottaken","nottaken"))

pneumo.es0$clear.est <- c(p.modela$estimates[2,1], p.modelb$estimates[2,1], p.modelc$estimates[2,1], p.modeld$estimates[2,1])
pneumo.es0$Lclear.est <- c(p.modela$L[2,1], p.modelb$L[2,1], p.modelc$L[2,1], p.modeld$L[2,1])
pneumo.es0$Uclear.est <- c(p.modela$U[2,1], p.modelb$U[2,1], p.modelc$U[2,1], p.modeld$U[2,1])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART+","NVT, HIV-","NVT, HIV+ART+"),
                         "abx" = c("taken", "taken", "nottaken","nottaken"))

pneumo.es1$clear.est <- c(p.modela$estimates[3,1], p.modelb$estimates[3,1], p.modelc$estimates[3,1], p.modeld$estimates[3,1])
pneumo.es1$Lclear.est <- c(p.modela$L[3,1], p.modelb$L[3,1], p.modelc$L[3,1], p.modeld$L[3,1])
pneumo.es1$Uclear.est <- c(p.modela$U[3,1], p.modelb$U[3,1], p.modelc$U[3,1], p.modeld$U[3,1])

pneumo <- rbind(pneumo.es0, pneumo.es1)

B <-
  ggplot(pneumo) + 
  geom_errorbar(aes(hivst,color=hivst,ymin=1/Lclear.est,ymax=1/Uclear.est,shape=abx),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,1/clear.est,color=hivst,shape=abx),size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  labs(title="B",x="",y="Average carriage duration by abx (days)") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  guides(color=guide_legend(title="Serotype, HIV status"),shape=guide_legend(title="On ABX"),fill=FALSE) +
  theme(legend.text=element_text(size=10),legend.position="right",legend.title=element_text(face="bold",size=10))
#-------------------------------------------------------

p.modela <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", season="hotwet"),ci="normal",cl=0.95)
p.modelb <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", season="hotwet"),ci="normal",cl=0.95)
p.modelc <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", season="cooldry"),ci="normal",cl=0.95)
p.modeld <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", season="cooldry"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART+","VT, HIV-","VT, HIV+ART+"), 
                         "season" = c("hotwet", "hotwet", "cooldry", "cooldry"))

pneumo.es0$clear.est <- c(p.modela$estimates[2,1], p.modelb$estimates[2,1], p.modelc$estimates[2,1], p.modeld$estimates[2,1])
pneumo.es0$Lclear.est <- c(p.modela$L[2,1], p.modelb$L[2,1], p.modelc$L[2,1], p.modeld$L[2,1])
pneumo.es0$Uclear.est <- c(p.modela$U[2,1], p.modelb$U[2,1], p.modelc$U[2,1], p.modeld$U[2,1])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART+","NVT, HIV-","NVT, HIV+ART+"),
                         "season" = c("hotwet", "hotwet", "cooldry", "cooldry"))

pneumo.es1$clear.est <- c(p.modela$estimates[3,1], p.modelb$estimates[3,1], p.modelc$estimates[3,1], p.modeld$estimates[3,1])
pneumo.es1$Lclear.est <- c(p.modela$L[3,1], p.modelb$L[3,1], p.modelc$L[3,1], p.modeld$L[3,1])
pneumo.es1$Uclear.est <- c(p.modela$U[3,1], p.modelb$U[3,1], p.modelc$U[3,1], p.modeld$U[3,1])

pneumo <- rbind(pneumo.es0, pneumo.es1)

C <-
  ggplot(pneumo) + 
  geom_errorbar(aes(hivst,color=hivst,ymin=1/Lclear.est,ymax=1/Uclear.est,shape=season),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,1/clear.est,color=hivst,shape=season),size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  labs(title="C",x="",y="Average carriage duration by season (days)") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  guides(color=guide_legend(title="Serotype, HIV status"),shape=guide_legend(title="season"),fill=FALSE) +
  theme(legend.text=element_text(size=10),legend.position="right",legend.title=element_text(face="bold",size=10))
#-------------------------------------------------------
p.modela <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", sex="Female"),ci="normal",cl=0.95)
p.modelb <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", sex="Female"),ci="normal",cl=0.95)
p.modelc <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", sex="Male"),ci="normal",cl=0.95)
p.modeld <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", sex="Male"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART+","VT, HIV-","VT, HIV+ART+"),
                         "sex" = c("Female", "Female", "Male", "Male"))

pneumo.es0$clear.est <- c(p.modela$estimates[2,1], p.modelb$estimates[2,1], p.modelc$estimates[2,1], p.modeld$estimates[2,1])
pneumo.es0$Lclear.est <- c(p.modela$L[2,1], p.modelb$L[2,1], p.modelc$L[2,1], p.modeld$L[2,1])
pneumo.es0$Uclear.est <- c(p.modela$U[2,1], p.modelb$U[2,1], p.modelc$U[2,1], p.modeld$U[2,1])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART+","NVT, HIV-","NVT, HIV+ART+"),
                         "sex" = c("Female", "Female", "Male", "Male"))

pneumo.es1$clear.est <- c(p.modela$estimates[3,1], p.modelb$estimates[3,1], p.modelc$estimates[3,1], p.modeld$estimates[3,1])
pneumo.es1$Lclear.est <- c(p.modela$L[3,1], p.modelb$L[3,1], p.modelc$L[3,1], p.modeld$L[3,1])
pneumo.es1$Uclear.est <- c(p.modela$U[3,1], p.modelb$U[3,1], p.modelc$U[3,1], p.modeld$U[3,1])

pneumo <- rbind(pneumo.es0, pneumo.es1)

D <-
  ggplot(pneumo) + 
  geom_errorbar(aes(hivst,color=hivst,ymin=1/Lclear.est,ymax=1/Uclear.est,shape=sex),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,1/clear.est,color=hivst,shape=sex),size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  labs(title="D",x="",y="Average carriage duration by sex (days)") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  guides(color=guide_legend(title="Serotype, HIV status"),shape=guide_legend(title="sex"),fill=FALSE) +
  theme(legend.text=element_text(size=10),legend.position="right",legend.title=element_text(face="bold",size=10))
#-------------------------------------------------------
p.modela <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", agegp="18-25"),ci="normal",cl=0.95)
p.modelb <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", agegp="18-25"),ci="normal",cl=0.95)
p.modelc <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", agegp="26-35"),ci="normal",cl=0.95)
p.modeld <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", agegp="26-35"),ci="normal",cl=0.95)
p.modele <- qmatrix.msm(p.model5, covariates=list(hiv="HIV-", agegp="36-45"),ci="normal",cl=0.95)
p.modelf <- qmatrix.msm(p.model5, covariates=list(hiv="HIV+ART+", agegp="36-45"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART+","VT, HIV-","VT, HIV+ART+", "VT, HIV-","VT, HIV+ART+"),
                         "agegp" = c("18-25", "18-25", "26-35", "26-35", "36-45", "36-45"))

pneumo.es0$clear.est <- c(p.modela$estimates[2,1], p.modelb$estimates[2,1], p.modelc$estimates[2,1], p.modeld$estimates[2,1],
                          p.modele$estimates[2,1], p.modelf$estimates[2,1])
pneumo.es0$Lclear.est <- c(p.modela$L[2,1], p.modelb$L[2,1], p.modelc$L[2,1], p.modeld$L[2,1], p.modele$L[2,1], p.modelf$L[2,1])
pneumo.es0$Uclear.est <- c(p.modela$U[2,1], p.modelb$U[2,1], p.modelc$U[2,1], p.modeld$U[2,1], p.modele$U[2,1], p.modelf$U[2,1])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART+","NVT, HIV-","NVT, HIV+ART+", "NVT, HIV-","NVT, HIV+ART+"),
                         "agegp" = c("18-25", "18-25", "26-35", "26-35", "36-45", "36-45"))

pneumo.es1$clear.est <- c(p.modela$estimates[3,1], p.modelb$estimates[3,1], p.modelc$estimates[3,1], p.modeld$estimates[3,1],
                          p.modele$estimates[3,1], p.modelf$estimates[3,1])
pneumo.es1$Lclear.est <- c(p.modela$L[3,1], p.modelb$L[3,1], p.modelc$L[3,1], p.modeld$L[3,1], p.modele$L[3,1], p.modelf$L[3,1])
pneumo.es1$Uclear.est <- c(p.modela$U[3,1], p.modelb$U[3,1], p.modelc$U[3,1], p.modeld$U[3,1], p.modele$U[3,1], p.modelf$U[3,1])

pneumo <- rbind(pneumo.es0, pneumo.es1)

E <-
  ggplot(pneumo) + 
  geom_errorbar(aes(hivst,color=hivst,ymin=1/Lclear.est,ymax=1/Uclear.est,shape=agegp),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,1/clear.est,color=hivst,shape=agegp),size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  labs(title="E",x="",y="Average carriage duration by sex (days)") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  guides(color=guide_legend(title="Serotype, HIV status"),shape=guide_legend(title="sex"),fill=FALSE) +
  theme(legend.text=element_text(size=10),legend.position="right",legend.title=element_text(face="bold",size=10))
#-------------------------------------------------------
#probability of clearance
p.modela <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV-"),ci="normal",cl=0.95)
p.modelb <- pmatrix.msm(p.model5,t=1,covariates=list(hiv="HIV+ART+"),ci="normal",cl=0.95)

pneumo.es0 <- data.frame("hivst" = c("VT, HIV-","VT, HIV+ART"))

pneumo.es0$clear.est <- c(p.modela$estimates[2,1], p.modelb$estimates[2,1])
pneumo.es0$Lclear.est <- c(p.modela$L[2,1], p.modelb$L[2,1])
pneumo.es0$Uclear.est <- c(p.modela$U[2,1], p.modelb$U[2,1])

pneumo.es1 <- data.frame("hivst" = c("NVT, HIV-","NVT, HIV+ART"))

pneumo.es1$clear.est <- c(p.modela$estimates[3,1], p.modelb$estimates[3,1])
pneumo.es1$Lclear.est <- c(p.modela$L[3,1], p.modelb$L[3,1])
pneumo.es1$Uclear.est <- c(p.modela$U[3,1], p.modelb$U[3,1])

pneumo <- rbind(pneumo.es0, pneumo.es1)

F <-
  ggplot(pneumo) + 
  geom_errorbar(aes(hivst,color=hivst,ymin=1/Lclear.est,ymax=1/Uclear.est),width=0,size=1,position=position_dodge(width=0.5)) +
  geom_point(aes(hivst,1/clear.est,color=hivst),shape=8,size=3,position=position_dodge(width=0.5),stat="identity") +
  theme_bw() + 
  labs(title="F",x="",y="probability of carriage clearance") + 
  theme(axis.text.y=element_text(face="bold",size=10)) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + 
  theme(legend.position="none")
#-------------------------------------------------------

print(ggarrange(A,B,C,D,ncol=4,common.legend=TRUE,legend="right"))
