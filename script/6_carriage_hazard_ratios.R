#written by Deus
#01/08/2022
#pneumococcal carriage ad serotype dynamics in HIV-infected adults in the infant PCV era

#====================================================================

#overview of hazard ratios for pneumococcal acquisition/clearance rates
H <- hazard.msm(spn_modelfit, hazard.scale = 1, cl = 0.95)

#create an empty acquisition table for forest plot
spn_hazard <- tibble(
  index = c(1:12),
  carr = c(rep(c("VT", "NVT"), 6)),
  cova = c(rep("hiv", 2), rep("agegp", 4), rep("sex", 2), rep("nochild", 2), rep("ses", 2)),
  label = c(rep("HIV- vs HIV+ on ART", 2), rep("Age group 18-24 vs 25-34", 2), rep("Age group 18-24 vs 35-45", 2), rep("Female vs male", 2), rep("1 Child vs 2+ children", 2), rep("High vs low SES", 2)),
  HR = c(rep(0, 12)),
  HRlci = c(rep(0, 12)),
  HRuci = c(rep(0, 12))
)

#insert HR values from fitted model

# for(i in 1:12){
#   for(j in c("hivHIV+ART+", "hivHIV+ART+", "agegp25-34", "agegp25-34", "agegp35-45", "agegp35-45", "sexMale", "sexMale", "nochild2+child", "nochild2+child", "seslow", "seslow")){
#     for(k in 1:2){
#       spn_hazard %>% 
#         mutate(HR = if_else(index == i, H[[j]][k], 
#     }
#     
#   }
# }

spn_hazard <-
spn_hazard %>% 
  mutate(HR = if_else(index == 1, H[["hivHIV+ART+"]][1],
                      if_else(index == 2, H[["hivHIV+ART+"]][2],
                              if_else(index == 3, H[["agegp25-34"]][1],
                                      if_else(index == 4, H[["agegp25-34"]][2],
                                              if_else(index == 5, H[["agegp35-45"]][1],
                                                      if_else(index == 6, H[["agegp35-45"]][2],
                                                              if_else(index == 7, H[["sexMale"]][1],
                                                                      if_else(index == 8, H[["sexMale"]][2],
                                                                              if_else(index == 9, H[["nochild2+child"]][1],
                                                                                      if_else(index == 10, H[["nochild2+child"]][2],
                                                                                              if_else(index == 11, H[["seslow"]][1],
                                                                                                      if_else(index == 12, H[["seslow"]][2], NA_real_)))))))))))))

                         
#insert HRlci values from fitted model
spn_hazard <-
spn_hazard %>% 
  mutate(HRlci = if_else(index == 1, H[["hivHIV+ART+"]][5],
                      if_else(index == 2, H[["hivHIV+ART+"]][6],
                              if_else(index == 3, H[["agegp25-34"]][5],
                                      if_else(index == 4, H[["agegp25-34"]][6],
                                              if_else(index == 5, H[["agegp35-45"]][5],
                                                      if_else(index == 6, H[["agegp35-45"]][6],
                                                              if_else(index == 7, H[["sexMale"]][5],
                                                                      if_else(index == 8, H[["sexMale"]][6],
                                                                              if_else(index == 9, H[["nochild2+child"]][5],
                                                                                      if_else(index == 10, H[["nochild2+child"]][6],
                                                                                              if_else(index == 11, H[["seslow"]][5],
                                                                                                      if_else(index == 12, H[["seslow"]][6], NA_real_)))))))))))))

#insert HRuci values from fitted model
spn_hazard <-
  spn_hazard %>% 
  mutate(HRuci = if_else(index == 1, H[["hivHIV+ART+"]][9],
                         if_else(index == 2, H[["hivHIV+ART+"]][10],
                                 if_else(index == 3, H[["agegp25-34"]][9],
                                         if_else(index == 4, H[["agegp25-34"]][10],
                                                 if_else(index == 5, H[["agegp35-45"]][9],
                                                         if_else(index == 6, H[["agegp35-45"]][10],
                                                                 if_else(index == 7, H[["sexMale"]][9],
                                                                         if_else(index == 8, H[["sexMale"]][10],
                                                                                 if_else(index == 9, H[["nochild2+child"]][9],
                                                                                         if_else(index == 10, H[["nochild2+child"]][10],
                                                                                                 if_else(index == 11, H[["seslow"]][9],
                                                                                                         if_else(index == 12, H[["seslow"]][10], NA_real_)))))))))))))

#====================================================================
spn_hazard %>%
ggplot(aes(y = label, x = HR, color = carr)) +
  geom_errorbarh(aes(xmin = HRlci, xmax = HRuci), height = 0.5, size = 1, position = position_dodge2(width = 0.5)) +
  geom_point(shape = 5, size = 3,  stroke = 1.4, position = position_dodge2(width = 0.5)) +  
  geom_vline(xintercept = 1, color = "black", linetype = "dashed", cex = 0.6, alpha = 0.8) +
  #scale_y_continuous(name = "", breaks=1:24, labels = spn_hazard$label, trans = "reverse") +
  scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6), limits = c(0, 6)) + 
  xlab("Hazard Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw(base_size = 14, base_family = 'Lato') +
  guides(color = guide_legend(title = "")) +
  theme(legend.text = element_text(size = 12), legend.position = c(0.7, 0.8), legend.title = element_text(size = 0)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
  

