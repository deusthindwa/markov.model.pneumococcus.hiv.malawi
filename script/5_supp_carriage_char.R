


S1 <- 
  pneumov %>%
  ungroup() %>%
  group_by(vday, sex, carr) %>%
  tally() %>%
  mutate(prev = n/sum(n)*100) %>%
  filter(vday != 0, vday <15) %>%
  mutate(obs_lci = exactci(n, sum(n), 0.95)$conf.int[1:2], obs_uci = exactci(n, sum(n), 0.95)$conf.int[3:4]) %>%
  filter(carr == 1) %>%
  
  ggplot() + 
  geom_point(aes(x = vday, y = prev, color = sex, size = n), shape = 1) +
  geom_line(aes(x = vday, y = prev, color = sex), size = 0.8) + 
  geom_ribbon(aes(x = vday, y = prev, group = sex, fill = sex, color = sex, ymin = obs_lci*100, ymax = obs_uci*100), alpha = 0.2, size = 0.1) +
  ylim(0,100) +
  scale_x_discrete(limits = c(0, 5, 10, 15)) +
  labs(title="S1", x="Visit number", y = "Carriage prevelance (%)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato") + 
  guides(fill = FALSE, color = guide_legend(title = "Sex"), size = guide_legend(title = "Sample size"))


S2 <- 
  pneumov %>%
  ungroup() %>%
  filter(vday != 0 & vday != 16, !is.na(dens)) %>%
  mutate(vday = as.factor(vday)) %>%
  
  ggplot(aes(x = vday, y = dens, fill = hiv)) + 
  geom_boxplot(notch = FALSE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  labs(title="S2", x="Visit days", y = "Pneumococcal carriage density (logCFU/ml)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato") + 
  guides(fill=guide_legend(title="HIV status")) + 
  scale_fill_manual("", values = c("HIV-" = "#C0C0C0", "HIV+ART+" = "#4B0082"))

S3 <- 
  pneumov %>%
  ungroup() %>%
  filter(serogroup != "None") %>%
  mutate(agegp = if_else(age >= 18 & age <= 25, "18 - 25", 
                         if_else(age >= 26 & age <= 35, "26 - 35", 
                                 if_else(age >= 36, "36 - 45", NA_character_)))) %>%
  
  ggplot(aes(x = agegp, y = dens, fill = hiv)) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  #facet_wrap(~serogroup) +
  labs(title="S3", x="Age group", y = "Pneumococcal carriage density (logCFU/ml)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato") + 
  guides(fill=guide_legend(title="HIV status")) 
  #scale_fill_manual("", values = c("HIV-" = "#C0C0C0", "HIV+ART+" = "#4B0082"))

S5 <-
  rbind(
    pneumov %>%
      ungroup() %>%
      group_by(hiv, serotype) %>%
      filter(serogroup != "NVT", serogroup != "None") %>%
      tally(),
    
    tibble(
      "hiv" = c("HIV-", "HIV-"),
      "serotype" = c("6A", "6B"),
      "n" = c(0, 0),
    )) %>%
  
  mutate(prev = n/sum(n)) %>% 
  mutate(obs_lci = if_else(prev !=0, exactci(n, sum(n), 0.95)$conf.int[1:10], NA_real_), 
         obs_uci = if_else(prev !=0, exactci(n, sum(n), 0.95)$conf.int[11:20], NA_real_)) %>%
  
  ggplot(aes(x = as_factor(serotype), y = prev, fill = hiv)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  scale_y_continuous(labels = scales::percent) +
  geom_errorbar(aes(ymin = obs_lci, ymax = obs_uci), width = 0.3, position = position_dodge(0.9), size = 0.8) +
  scale_x_discrete(limits=c("3","4","6A","6B","9V","14","18C","19A", "19F", "23F"))  +
  labs(title="S5", x="Serotypes", y = "Proportion of PCV13 serotypes") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato") + 
  guides(fill=guide_legend(title = "HIV status"))

ggsave(here("output", "S1_carriage_char.png"),
       plot = ((S1 | S2 | S3 )),
       width = 21, height = 5, unit="in", dpi = 300)

S6 <- 
  pneumov %>%
  ungroup() %>%
  mutate_if(is.numeric, round) %>%
  mutate(artdur = if_else(artdur <= 3, "short", 
                          if_else(artdur > 3, "long", NA_character_))) %>%
  mutate(artdur = as.factor(artdur)) %>%
  filter(!is.na(artdur)) %>%
  
  ggplot(aes(x = artdur, y = dens)) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  labs(title="S6", x="ART duration (Years)", y = "Pneumococcal carriage density (logCFU/ml)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato")
  
S7 <- 
  pneumov %>%
  ungroup() %>%
  mutate_if(is.numeric, round) %>%
  mutate(cd4 = if_else(cd4 <= 400, "low", 
                if_else(cd4 > 400, "high", NA_character_))) %>%
  mutate(cd4 = as.factor(cd4)) %>%
  filter(!is.na(cd4), hiv != "HIV-") %>%
  
  ggplot(aes(x = cd4, y = dens)) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  labs(title="S7", x="CD4 count (cells/µl)", y = "Pneumococcal carriage density (logCFU/ml)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato")

S8 <- 
  pneumov %>%
  ungroup() %>%
  mutate_if(is.numeric, round) %>%
  mutate(ses = if_else(ses <= 3, "Low", 
                       if_else(ses > 3, "high", NA_character_))) %>%
  mutate(ses = as.factor(ses)) %>%
  filter(!is.na(ses)) %>%
  
  ggplot(aes(x = ses, y = dens, fill = hiv)) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  labs(title="S8", x="Social economic status", y = "Pneumococcal carriage density (logCFU/ml)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato")

S9 <- 
  pneumov %>%
  ungroup() %>%
  
  ggplot(aes(x = sex, y = dens, fill = hiv)) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  labs(title="S9", x="Gender", y = "Pneumococcal carriage density (logCFU/ml)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato")

S10 <- 
  pneumov %>%
  ungroup() %>%
  mutate(season = month(as.POSIXlt(date, format="%d/%m/%Y")),
         
         season = if_else(season >= 5 & season <= 10, "cooldry", 
                          if_else(season <= 4, "hotwet", "hotwet", NA_character_))) %>%
  
  ggplot(aes(x = season, y = dens, fill = hiv)) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  labs(title="S10", x="Season", y = "Pneumococcal carriage density (logCFU/ml)") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato")

S11 <- 
  pneumov %>%
  ungroup() %>%
  mutate(season = month(as.POSIXlt(date, format="%d/%m/%Y")),
         
         season = if_else(season >= 5 & season <= 10, "cooldry", 
                          if_else(season <= 4, "hotwet", "hotwet", NA_character_))) %>%
  group_by(hiv, season, carr) %>%
  tally() %>%
  mutate(prev = n/sum(n)*100) %>%
  
  ggplot(aes(x = season, y = prev, fill = hiv)) + 
  geom_boxplot(notch = FALSE, color = "black", size = 1) +
  scale_y_continuous(trans = log10_trans()) +
  labs(title="S10", x="Season", y = "Carriage prevelance") +
  theme(plot.title = element_text(size = 18), axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(legend.text=element_text(size = 11), legend.title = element_text(size = 11)) + 
  theme_bw(base_size = 14, base_family = "Lato")