############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 2:                             ###
###                        BERECHNUNG HYPOTHESE 1                        ###
###                                                                      ###
############################################################################
############################################################################

# Hypothese 1: Die Implementierung aktiver Erklär- und Reflexionstätigkeit in ein 
# sport-fachdidaktisches Seminar führt im Vergleich zur rein passiven Beobachtung 
# von Erklärtätigkeit zu signifikanten größeren Verbesserungen in der makro-adaptiven 
# Erklärkompetenz.

data_H1 <- subset(data, gruppe == 0 | gruppe == 1)

### Einfluss von Gruppe und Messzeitpunkt auf die Globalnote mit Grafiken
# Deskriptive Statistik
data_H1 %>% 
  mutate(
    Messzeitpunkt = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2'))
  ) %>%
  group_by(Messzeitpunkt, gruppe) %>% 
  summarise(mean = mean(cke_glo, na.rm = T),
            SE = sd(cke_glo, na.rm = T)/sqrt(n())
  )

# Mixed Model
lmer(cke_glo ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = data_H1) %>% 
    summary

# Grafiken
data_H1 %>% 
  mutate(MZP = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2')), 
         Gruppe = factor(if_else(gruppe == 0, 'TG 1 (N = 27)', 'TG 2 (N = 26)'))) %>%
  group_by(MZP, Gruppe) %>% 
  summarise(Globalnote = mean(cke_glo, na.rm = T), SE = sd(cke_glo, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = MZP, y = Globalnote, ymax = Globalnote + SE, ymin = Globalnote - SE, group = Gruppe)) +
  stat_summary(fun = identity, geom = 'line', aes(color = Gruppe, linetype = Gruppe), size = 1) +
  geom_point(aes(color = Gruppe, shape = Gruppe), size = 0.5) +
  geom_errorbar(aes(color = Gruppe), width = .05, size = 1) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  geom_text(data = . %>% filter(MZP == "MZP 1"),
            aes(label = round(Globalnote, 2), y = Globalnote, group = Gruppe, color = Gruppe),
            position = position_nudge(x = -0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = . %>% filter(MZP == "MZP 2"),
            aes(label = round(Globalnote, 2), y = Globalnote, group = Gruppe, color = Gruppe),
            position = position_nudge(x = 0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  theme_bw() +
  theme(
    legend.position="bottom",
    panel.grid = element_blank(),
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10))

  
data_H1 %>% 
  mutate(mzp = factor(mzp), gruppe = factor(gruppe)) %>%
  group_by(mzp, gruppe) %>% 
  ggplot(.) +
  aes(x = mzp, y = cke_glo) +
  geom_boxplot() +
  facet_wrap(~ gruppe)

### Einfluss von Gruppe und Messzeitpunkt auf den z-standardisierten Durchschnittswert
### aller relevanten Ratingwerte mit Grafiken

# Deskriptive Statistik
data_H1 %>% 
  mutate(
    Messzeitpunkt = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2'))
  ) %>%
  group_by(Messzeitpunkt, gruppe) %>% 
  summarise(mean = mean(cke_ges, na.rm = T),
            SE = sd(cke_ges, na.rm = T)/sqrt(n())
  )

# Mixed Model
data_H1 %>% 
  lmer(cke_ges ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary

# Grafiken
data_H1 %>% 
  mutate(MZP = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2')), 
         Gruppe = factor(if_else(gruppe == 0, 'TG 1 (N = 27)', 'TG 2 (N = 26)'))) %>%
  group_by(MZP, Gruppe) %>% 
  summarise(Gesamturteil = mean(cke_ges, na.rm = T), SE = sd(cke_ges, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = MZP, y = Gesamturteil, ymax = Gesamturteil + SE, ymin = Gesamturteil - SE, group = Gruppe)) +
  stat_summary(fun = identity, geom = 'line', aes(color = Gruppe, linetype = Gruppe), size = 1) +
  geom_point(aes(color = Gruppe, shape = Gruppe), size = 0.5) +
  geom_errorbar(aes(color = Gruppe), width = .05, size = 1) +
  scale_color_manual(values = c("#d95f02", "#1b9e77")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  geom_text(data = . %>% filter(MZP == "MZP 1"),
            aes(label = round(Gesamturteil, 2), y = Gesamturteil, group = Gruppe, color = Gruppe),
            position = position_nudge(x = -0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = . %>% filter(MZP == "MZP 2"),
            aes(label = round(Gesamturteil, 2), y = Gesamturteil, group = Gruppe, color = Gruppe),
            position = position_nudge(x = 0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  theme_bw() +
  theme(
    legend.position="bottom",
    panel.grid = element_blank(),
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10))

data_H1 %>% 
  mutate(mzp = factor(mzp), gruppe = factor(gruppe)) %>%
  group_by(mzp, gruppe) %>% 
  ggplot(.) +
  aes(x = mzp, y = cke_ges) +
  geom_boxplot() +
  facet_wrap(~ gruppe)


# Einfluss von Gruppe und Messzeitpunkt auf die einzelnen z-standardisierten
# Durchschnittswerte aller Variablen eines Kriteriums guten Erklärens
data_H1 %>% 
  lmer(cke_st ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary

data_H1 %>% 
  lmer(cke_sy ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary

data_H1 %>% 
  lmer(cke_ad ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary

data_H1 %>% 
  lmer(cke_vi ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary
