############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 3:                             ###
###                        BERECHNUNG HYPOTHESE 2                        ###
###                                                                      ###
############################################################################
############################################################################

# Hypothese 2: Die Implementierung aktiver und passiver Erklärinhalte in ein 
# sport-fachdidaktisches Seminar führt im Vergleich zu einem Seminar zur 
# fachdidaktischen Bewegungsvermittlung zu signifikant größeren Verbesserungen 
# in der makro-adaptiven Erklärkompetenz

data_H2 <- subset(data, gruppe == 0 | gruppe == 1 | gruppe == 2) %>% 
  mutate(status = ifelse(gruppe < 2, "Treatment", "Kontrolle"))

### Einfluss von Gruppe und Messzeitpunkt auf die Globalnote mit Grafiken
# Deskriptive Statistik
data_H2 %>% 
  mutate(
    Messzeitpunkt = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2'))
  ) %>%
  group_by(Messzeitpunkt, status) %>% 
  summarise(mean = mean(cke_glo, na.rm = T),
            SE = sd(cke_glo, na.rm = T)/sqrt(n())
  )

# Mixed Model
lmer(cke_glo ~ mzp + status + mzp:status + (1 | cod_stu), data = data_H2) %>% 
  summary

# Grafiken
data_H2 %>% 
  mutate(MZP = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2')), 
         Status = factor(if_else(status == 'Treatment', 'Treatmentgruppe (N = 53)', 'Kontrollgruppe (N = 28)'))) %>%
  group_by(MZP, Status) %>% 
  summarise(Globalnote = mean(cke_glo, na.rm = T), SE = sd(cke_glo, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = MZP, y = Globalnote, ymax = Globalnote + SE, ymin = Globalnote - SE, group = Status)) +
  stat_summary(fun = identity, geom = 'line', aes(color = Status, linetype = Status), size = 1) +
  geom_point(aes(color = Status, shape = Status), size = 0.5) +
  geom_errorbar(aes(color = Status), width = .05, size = 1) +
  scale_color_manual(values = c("#0077be", "#bf5b17")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  geom_text(data = . %>% filter(MZP == "MZP 1"),
            aes(label = round(Globalnote, 2), y = Globalnote, group = Status, color = Status),
            position = position_nudge(x = -0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = . %>% filter(MZP == "MZP 2"),
            aes(label = round(Globalnote, 2), y = Globalnote, group = Status, color = Status),
            position = position_nudge(x = 0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  theme_bw() +
  theme(
    legend.position="bottom",
    panel.grid = element_blank(),
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10))

data_H2 %>% 
  group_by(mzp, gruppe) %>% 
  ggplot(.) +
  aes(x = mzp, y = cke_glo) +
  geom_boxplot() +
  facet_wrap(~ gruppe)

### Einfluss von Gruppe und Messzeitpunkt auf den z-standardisierten Durchschnittswert
### aller relevanten Ratingwerte mit Grafiken
# Deskriptive Statistik
data_H2 %>% 
  mutate(
    Messzeitpunkt = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2'))
  ) %>%
  group_by(Messzeitpunkt, status) %>% 
  summarise(mean = mean(cke_ges, na.rm = T),
            SE = sd(cke_ges, na.rm = T)/sqrt(n())
  )

# Mixed Model
lmer(cke_ges ~ mzp + status + mzp:status + (1 | cod_stu), data = data_H2) %>% 
  summary

# Grafiken
data_H2 %>% 
  mutate(MZP = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2')), 
         Status = factor(if_else(status == 'Treatment', 'Treatmentgruppe (N = 53)', 'Kontrollgruppe (N = 28)'))) %>%
  group_by(MZP, Status) %>% 
  summarise(Gesamturteil = mean(cke_ges, na.rm = T), SE = sd(cke_ges, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = MZP, y = Gesamturteil, ymax = Gesamturteil + SE, ymin = Gesamturteil - SE, group = Status)) +
  stat_summary(fun = identity, geom = 'line', aes(color = Status, linetype = Status), size = 1) +
  geom_point(aes(color = Status, shape = Status), size = 0.5) +
  geom_errorbar(aes(color = Status), width = .05, size = 1) +
  scale_color_manual(values = c("#0077be", "#bf5b17")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  geom_text(data = . %>% filter(MZP == "MZP 1"),
            aes(label = round(Gesamturteil, 2), y = Gesamturteil, group = Status, color = Status),
            position = position_nudge(x = -0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = . %>% filter(MZP == "MZP 2"),
            aes(label = round(Gesamturteil, 2), y = Gesamturteil, group = Status, color = Status),
            position = position_nudge(x = 0.1), size = 4, na.rm = TRUE, show.legend = FALSE) +
  theme_bw() +
  theme(
    legend.position="bottom",
    panel.grid = element_blank(),
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10))

data_H2 %>% 
  mutate(mzp = factor(mzp), gruppe = factor(gruppe)) %>%
  group_by(mzp, gruppe) %>% 
  ggplot(.) +
  aes(x = mzp, y = cke_ges) +
  geom_boxplot() +
  facet_wrap(~ gruppe)

# Einfluss von Gruppe und Messzeitpunkt auf die einzelnen z-standardisierten
# Durchschnittswerte aller Variablen eines Kriteriums guten Erklärens
data_H2 %>% 
  lmer(cke_st ~ mzp + status + mzp:status + (1 | cod_stu), data = .) %>% 
  summary

data_H2 %>% 
  lmer(cke_sy ~ mzp + status + mzp:status + (1 | cod_stu), data = .) %>% 
  summary

data_H2 %>% 
  lmer(cke_ad ~ mzp + status + mzp:status + (1 | cod_stu), data = .) %>% 
  summary

data_H2 %>% 
  lmer(cke_vi ~ mzp + status + mzp:status + (1 | cod_stu), data = .) %>% 
  summary
