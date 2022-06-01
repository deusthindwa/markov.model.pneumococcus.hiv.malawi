# written by Lusako and Deus
# 01/03/2022
# pneumococcal carriage in HIV-infected adults in PCV era

# prepare covariates and outcome varoables for modelling
 
# outcome
pneumodel <- pneumov

pneumodel <- pneumodel %>%

  mutate(dys = day,
         
         state = if_else(serogroup == "None", 1L, if_else(serogroup == "VT", 2L, 3L)),
         
         nochild = if_else(nochild == 1, "1 child", "2+ children"),
         
         abx = if_else(abx == 1, "taken", if_else(abx == 0, "nottaken", NA_character_)),
         
         cd4 = if_else(cd4 <= 200, "low", 
                       if_else(cd4 > 200 & cd4 <= 500, "medium", 
                               if_else(cd4 > 500, "high", NA_character_))),
         
         artdur = if_else(is.na(artdur), "none",  
                          if_else(artdur <= 3, "short", "long")),
         
         #artdur = if_else(artdur <=3, "short",  
        #                  if_else(artdur > 3, "long", NA_character_)),
                          
         dens = if_else(dens <= 1675, "low", if_else(dens > 1675, "high", NA_character_)),
         
         ses = if_else(ses <= 3, "Low", if_else(ses > 3, "high", NA_character_)),
         
         agegp = if_else(age >= 18 & age <= 25, "18-25", 
                         if_else(age > 25 & age <= 35, "26-35", 
                                 if_else(age > 35, "36-45", NA_character_))),
         
         season = if_else(month(date) >= 5 & month(date) <= 10, "cooldry", 
                          if_else(month(date) <= 4, "hotwet", 
                                  if_else(month(date) > 10, "hotwet", NA_character_)))
      )  

pneumodel <- pneumodel %>% select(pid, dys, vday, state, hiv, cd4, artdur, sex, agegp, dens, season, abx, ses, nochild)

# show transition frequency
pneumodel <- arrange(pneumodel, pid, dys)
statetable.msm(state, pid, data = pneumodel)

#initiate transition intensity matrix Q
matrix.Q <- rbind(c(0.03, 0.12, 0.26),
                  c(0.26, 0.74, 0.0), 
                  c(0.15, 0.00, 0.35))

rownames(matrix.Q) <- c("Clear", "VT_carry", "NVT_carry")
colnames(matrix.Q) <- c("Clear", "VT_carry", "NVT_carry")

matrix.Q

#run the Markov models
p.model1 <- msm(state ~ dys, subject = pid, data = pneumodel,
                qmatrix = matrix.Q,
                covariates = list("1-2" = ~ hiv + agegp + sex + nochild + season + ses, "2-1" =~ hiv + agegp + sex + season + abx + cd4, 
                                  "1-3" = ~ hiv + agegp + sex + nochild + season + ses, "3-1" =~ hiv + agegp + sex + season + abx + cd4),
               opt.method = "bobyqa", control = list(maxfun = 1000000))

#run the Markov models
p.model2 <- msm(state ~ dys, subject = pid, data = pneumodel,
               qmatrix = matrix.Q,
               covariates = list("1-2" = ~ hiv + agegp + sex + nochild + season + ses, "2-1" =~ hiv + agegp + sex + season + abx + dens, 
                                 "1-3" = ~ hiv + agegp + sex + nochild + season + ses, "3-1" =~ hiv + agegp + sex + season + abx + dens),
               opt.method = "bobyqa", control = list(maxfun = 1000000))

#run the Markov models
p.model3 <- msm(state ~ dys, subject = pid, data = pneumodel,
                qmatrix = matrix.Q,
                covariates = list("1-2" = ~ hiv + agegp + sex + nochild + season + ses, "2-1" =~ agegp + sex + season + abx + artdur + cd4, 
                                  "1-3" = ~ hiv + agegp + sex + nochild + season + ses, "3-1" =~  agegp + sex + season + abx + artdur + cd4),
                opt.method = "bobyqa", control = list(maxfun = 10000000))


#comparing the AIC scores of the fitted models 
AIC(p.model1, p.model2, p.model3, p.model4, p.model5, p.model6, p.model7, p.model8)

#Model comparison (Likelihood ratio tests)
lrtest(p.model1, p.model2, p.model3, p.model4, p.model5, p.model6, p.model7, p.model8)

#current model of choice
p.model5