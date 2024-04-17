############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 4:                             ###
###                        BERECHNUNG HYPOTHESE 3                        ###
###                                                                      ###
############################################################################
############################################################################

# Hypothese 3: Die Implementierung aktiver und passiver Erklärinhalte in ein 
# sport-fachdidaktisches Seminar führt im Vergleich zu einem Seminar zur 
# fachdidaktischen Bewegungsvermittlung zu einer kürzeren Erklärdauer bei 
# gleichbleibender Erklärqualität

data_H3 <- subset(data, gruppe == 0 | gruppe == 1 | gruppe == 2)

# Grafiken zur Erklärdauer nach Gruppen und Messzeitpunkten
variable_names <- list(
    "0" = "TG 1",
    "1" = "TG 2",
    "2" = "KG"
    )
variable_labeller <- function(variable,value){
   return(variable_names[value])
}

data_H3 %>% 
  mutate(mzp = factor(mzp), gruppe = factor(gruppe)) %>%
  group_by(mzp, gruppe) %>% 
  ggplot(.) +
  aes(x = mzp, y = cke_times) +
  geom_boxplot() +
  ggtitle(NULL) +
  xlab("") +
  ylab("Erklärzeit in Sekunden") +
  facet_wrap(~ gruppe, labeller=variable_labeller)

data_H3 %>% 
  mutate(MZP = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2')), 
         Gruppe = factor(if_else(gruppe == 0, 'TG 1(N = 27)', 
                                 if_else(gruppe == 1, 'TG 2 (N = 26)', 
                                         'KG (N = 28)')))) %>%
  group_by(MZP, Gruppe) %>% 
  summarise(Erklärzeit = mean(cke_times, na.rm = T), SE = sd(cke_times, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = MZP, y = Erklärzeit, ymax = Erklärzeit + SE, ymin = Erklärzeit - SE, group = Gruppe)) +
  stat_summary(fun = identity, geom = 'line', aes(color = Gruppe, linetype = Gruppe), size = 1) +
  geom_point(aes(color = Gruppe, shape = Gruppe), size = 0.5) +
  geom_errorbar(aes(color = Gruppe), width = .05, size = 1) +
  scale_color_manual(values = c("#0077be", "#009B77", "orange")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  geom_text(data = . %>% filter(MZP == "MZP 1"),
            aes(label = round(Erklärzeit, 2), y = Erklärzeit, group = Gruppe, color = Gruppe),
            position = position_nudge(x = -0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = . %>% filter(MZP == "MZP 2"),
            aes(label = round(Erklärzeit, 2), y = Erklärzeit, group = Gruppe, color = Gruppe),
            position = position_nudge(x = 0.1, y = c(-0.5, 0, 0.5)), size = 4, na.rm = TRUE, show.legend = FALSE) +
  theme_bw() +
  theme(
    legend.position="bottom",
    panel.grid = element_blank(),
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10))


### Einfluss von Gruppe und Messzeitpunkt auf die Erklärdauer mit Grafiken
# Deskriptive Statistik
data_H3 %>% 
  mutate(
    Messzeitpunkt = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2'))
  ) %>%
  group_by(Messzeitpunkt, gruppe) %>% 
  summarise(mean = mean(cke_times, na.rm = T),
            SE = sd(cke_times, na.rm = T)/sqrt(n())
  )

# Mixed Model
data_H3 %>% 
  mutate(status = ifelse(gruppe < 2, "Treatment", "Kontrolle")) %>% 
  lmer(cke_times ~ mzp + status + mzp:status + (1 | cod_stu), data = .) %>% 
  summary

### Einfluss von Erklärdauer und Status auf den z-standardisierten 
### Durchschnittswert aller relevanten Ratingwerte mit Grafiken
# Deskriptive Statistik
subset(data_H3, gruppe < 2 & cke_times < 60) %>% 
  nrow
subset(data_H3, gruppe < 2 & cke_times > 60) %>% 
  nrow

subset(data_H3, gruppe == 2 & cke_times < 60) %>% 
  nrow
subset(data_H3, gruppe == 2 & cke_times > 60) %>% 
  nrow

# Mixed Model
data_H3 %>% 
  mutate(status = ifelse(gruppe < 2, "Treatment", "Kontrolle")) %>%
  lmer(cke_ges ~ mzp + cke_times + cke_times:mzp + (1 | cod_stu), data = .) %>% 
  summary

# Grafiken nach Gruppe
data_H3 %>%
  mutate(MZP = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2')), 
         Gruppe = factor(if_else(gruppe == 0, 'TG 1(N = 27)', 
                                 if_else(gruppe == 1, 'TG 2 (N = 26)', 
                                         'KG (N = 28)')))) %>%
  group_by(MZP, Gruppe) %>% 
  summarise(Erklärzeit = mean(cke_times, na.rm = T), Gesamturteil = mean(cke_ges, na.rm = T)) %>%
  ggplot(aes(x = MZP, y = Gesamturteil)) +
  scale_y_continuous(
    name = "Gesamturteil",
    sec.axis = sec_axis(trans=~((. * 100) + 60), name="Erklärdauer")) +
  geom_point(aes(y = Gesamturteil), size = 2, color = "blue") +
  geom_point(aes(y = (Erklärzeit - 60) / 100), size = 2, color = "red") +
  facet_wrap(~ Gruppe) +
  theme(axis.title.y.right = element_text(colour = "red"),
        axis.title.y.left = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "red"),
        axis.text.y.left = element_text(color = "blue"),
        legend.position = "none",
        axis.title.x = element_blank()
  )
  
 