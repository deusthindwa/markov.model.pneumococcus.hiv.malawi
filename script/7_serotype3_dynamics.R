#written by Deus
#01/08/2022
#pneumococcal carriage ad serotype dynamics in HIV-infected adults in the infant PCV era

#====================================================================

#baseline serotype frequency

#A <- 
  spn_baseline %>% 
  group_by(serotype) %>%
  tally() %>%
  ggplot(aes(x = serotype), fill = serotype) + 
  geom_bar(aes(y = n), color = "black", stat = 'identity') + 
  theme_bw() + 
  labs(title = "A", x = "Pneumococcal serotypes", y = "Frequency") +
  scale_fill_grey(start = 0.9, end = 0.2) +
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold",size = 10)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 10), axis.title.y=element_text(face = "bold", size = 10)) +
  theme(legend.position = "none")

spn_fup %>% 
  group_by(vday, serotype) %>% tally() %>%
  ggplot(mapping = aes(x = `as.integer(year(ymd(pcvpa.des$date)))`, y = serogroup, fill = n)) +
  geom_point(aes(size = n, color = n), shape = 21) +
  theme_bw() +
  labs(title = "D", x = "Year of survey", y = "Serogroup") +
  scale_fill_continuous(type = "viridis") +
  scale_color_continuous(type = "viridis") +  
  theme(axis.text.x = element_text(face = "bold", size = 13), axis.text.y = element_text(face = "bold", size = 13)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size=13), axis.title.y = element_text(face = "bold", size = 13)) +
  guides(fill = guide_legend(title = ""), size = guide_legend(title = "")) + 
  theme(legend.position = "bottom")



