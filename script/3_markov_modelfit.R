#written by Deus
#01/08/2022
#pneumococcal carriage ad serotype dynamics in HIV-infected adults in the infant PCV era

#====================================================================

#select potential covariates and outcomes for SIS markov modelling
spn_model <- 
  spn_fup %>%
  mutate(pid = pid,
         
         dys = day,
         
         vday = as.integer(vday),
         
         state = as.integer(if_else(serogroup == "None", 1L, 
                                    if_else(serogroup == "VT", 2L, 3L))),
         
         sex = as.factor(sex),
         
         agegp = as.factor(if_else(age >= 18 & age <= 24, "18-24", 
                                   if_else(age > 24 & age <= 34, "25-34", 
                                           if_else(age > 34 & age <= 45, "35-45", NA_character_)))),
         
         hiv = as.factor(hiv),
         
         artdur = as.factor(if_else(artdur <=3, "short", 
                                    if_else(artdur > 3 & artdur <= 25, "long", NA_character_))),
         
         cd4 = as.factor(if_else(cd4 <= 200, "low", #categorised similarly for HIVpos and HIVneg?
                       if_else(cd4 > 200 & cd4 <= 500, "medium", 
                               if_else(cd4 > 500, "high", NA_character_)))),
                          
         dens = as.factor(if_else(carr == 0, "none",
                        if_else(dens <= 1675, "low", #1st quantile
                                if_else(dens > 1675 & dens <= 1206000000, "high", NA_character_)))), #2nd quantile

         season = as.factor(if_else(month(date) >= 5 & month(date) <= 10, "cooldry", 
                                    if_else(month(date) <= 4, "hotwet", 
                                            if_else(month(date) > 10, "hotwet", NA_character_)))),
         
         abx = as.factor(if_else(abx == 1, "taken", 
                                 if_else(abx == 0, "nottaken", NA_character_))),
         
         ses = as.factor(if_else(ses <= 3, "low", 
                       if_else(ses > 3 & ses <=15, "high", NA_character_))),
         
         nochild = as.factor(if_else(nochild == 1, "1child", "2+child")),
         
         pcicov = as.integer(if_else(dys <= 60, 1L,
                                    if_else(dys > 60 & dys <= 120, 2L,
                                            if_else(dys > 120 & dys <= 180, 3L,
                                                    if_else(dys > 180 & dys <= 240, 4L, 5L)))))
         
         # mstate = if_else(is.na(serotype), 1L,
         #                  if_else(serotype == "3", 2L,
         #                          if_else(serotype == "6A", 3L,
         #                                  if_else(serotype == "6B", 3L,
         #                                          if_else(serotype == "9V", 4L,
         #                                                  if_else(serotype == "19A", 5L,
         #                                                          if_else(serotype == "19F", 5L,
         #                                                                  if_else(serotype %in% c("4", "5", "7F", "14", "18C", "23F"), 6L, 7L)))))))))
         
      ) %>%
  select(pid, dys, vday, state, sex, agegp, hiv, artdur, cd4, dens, season, abx, ses, nochild, serotype, pcicov)

#====================================================================
# CONTINUOUS-TIME TIME-NONHOMOGENEOUS MARKOV MODEL
#====================================================================

# show transition frequency
spn_model <- arrange(spn_model, pid, dys)
statetable.msm(state, pid, data = spn_model)

#initiate transition intensity matrix Q
spn_Qmatrix <- rbind(c(0.03, 0.12, 0.26),
                     c(0.26, 0.74, 0.00), 
                     c(0.15, 0.00, 0.35))

rownames(spn_Qmatrix) <- c("Clear", "VT_carry", "NVT_carry")
colnames(spn_Qmatrix) <- c("Clear", "VT_carry", "NVT_carry")
spn_Qmatrix

#run the Markov model
spn_modelfit <- msm(state ~ dys, subject = pid, data = spn_model,
                qmatrix = spn_Qmatrix,
                covariates = list("1-2" = ~ hiv + agegp + sex + nochild + ses + pcicov, "2-1" =~ hiv + agegp + sex + dens + abx, 
                                  "1-3" = ~ hiv + agegp + sex + nochild + ses + pcicov, "3-1" =~ hiv + agegp + sex + dens + abx),
                #pci = c(60, 120, 180, 240),
                #pci = c(30, 60, 90, 120, 130, 180, 210, 240),
                pci = c(180),
                opt.method = "bobyqa", control = list(maxfun = 1000000))

# #====================================================================
# CONTINUOUS-TIME TIME-NONHOMOGENEOUS MARKOV MODEL (VT SEROTYPE)
# #====================================================================
# 
# show transition frequency
# spn_model <- arrange(spn_model, pid, dys)
# statetable.msm(mstate, pid, data = spn_model)
# 
# #initiate transition intensity matrix Q
# spn_Qmatrix <- rbind(c(0.26, 0.26, 0.26, 0.26, 0.26, 0.26, 0.26),
#                      c(0.26, 0.26, 0.00, 0.00, 0.00, 0.00, 0.00), 
#                      c(0.26, 0.00, 0.26, 0.00, 0.00, 0.00, 0.00),
#                      c(0.26, 0.00, 0.00, 0.26, 0.00, 0.00, 0.00),
#                      c(0.26, 0.00, 0.00, 0.00, 0.26, 0.00, 0.00),
#                      c(0.26, 0.00, 0.00, 0.00, 0.00, 0.26, 0.00),
#                      c(0.26, 0.00, 0.00, 0.00, 0.00, 0.00, 0.26))
# 
# rownames(spn_Qmatrix) <- c("clear", "s3", "s6", "9", "s19", "vt", "nvt")
# colnames(spn_Qmatrix) <- c("clear", "s3", "s6", "9", "s19", "vt", "nvt")
# 
# spn_Qmatrix
# 
# #run the Markov model
# spn_modelfitS <- msm(mstate ~ dys, subject = pid, data = spn_model,
#                     qmatrix = spn_Qmatrix,
#                     covariates = list("1-2" = ~ hiv + agegp + sex + nochild + ses, "2-1" =~ hiv + agegp + sex + dens + abx, 
#                                       "1-3" = ~ hiv + agegp + sex + nochild + ses, "3-1" =~ hiv + agegp + sex + dens + abx),
#                     pci = c(60, 120, 180, 240),
#                     opt.method = "bobyqa", control = list(maxfun = 1000000))
# 