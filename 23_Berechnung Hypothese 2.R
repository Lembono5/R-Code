############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 3:                             ###
###                        BERECHNUNG HYPOTHESE 2                        ###
###                                                                      ###
############################################################################
############################################################################

# Hypothese 2: Die Implementierung eigener Erklärtätigkeiten und videobasierter 
# Reflexion einer eigenen Unterrichtsminiatur in ein sport-fachdidaktisches Seminar 
# führt zu signifikant größeren Verbesserungen in den Unterrichtsplanungen als 
# Unterrichtsbeobachtung und Betrachtung eines fremden Videos.

data_H2 <- subset(data, gruppe < 2, select = c(cod_stu, mzp, gruppe, cep_glo, cep_ges, cep_st, cep_av, cep_ad, cep_fs))

# Deskriptive Statistik
data_H2 %>% 
  mutate(
    Messzeitpunkt = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2'))
  ) %>%
  group_by(Messzeitpunkt, gruppe) %>% 
  summarise(mean = mean(cep_ges, na.rm = T),
            SE = sd(cep_ges, na.rm = T)/sqrt(n())
  )

# Mixed Model
lmer(cep_ges ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = data_H2) %>% 
  summary

# Grafiken
data_H2 %>% 
  mutate(MZP = factor(if_else(mzp == 0, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2')), 
         Gruppe = factor(if_else(gruppe == 0, 'TG 1 (N = 27)', 'TG 2 (N = 26)'))) %>%
  group_by(MZP, Gruppe) %>% 
  summarise(Gesamturteil = mean(cep_ges, na.rm = T), SE = sd(cep_ges, na.rm = T)/sqrt(n())) %>%
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

data_H2 %>%
  group_by(mzp, gruppe) %>% 
  summarise(cep_ges = mean(cep_ges, na.rm = T), SE = sd(cep_ges, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = mzp, y = cep_ges, color = gruppe, ymax = cep_ges + SE, ymin = cep_ges - SE, group = gruppe)) +
  stat_summary(fun = identity, geom = 'line', show.legend = T) + 
  geom_point(show.legend = T) + geom_errorbar(width = .2)

data_H2 %>%
  group_by(mzp, gruppe) %>% 
  summarise(cep_glo = mean(cep_glo, na.rm = T), SE = sd(cep_glo, na.rm = T)/sqrt(n())) %>%
  ggplot(aes(x = mzp, y = cep_glo, color = gruppe, ymax = cep_glo + SE, ymin = cep_glo - SE, group = gruppe)) +
  stat_summary(fun = identity, geom = 'line', show.legend = T) + 
  geom_point(show.legend = T) + geom_errorbar(width = .2)


# Einfluss von Gruppe und Messzeitpunkt auf den z-standardisierten Durchschnittswert
# aller relevanten Ratingwerte mit Grafiken
data_H2 %>% 
  lmer(cep_glo ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary


# Einfluss von Gruppe und Messzeitpunkt auf die einzelnen z-standardisierten
# Durchschnittswerte aller Variablen eines Kriteriums guten Erklärens
data_H2 %>% 
  lmer(cep_st ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary

data_H2 %>% 
  lmer(cep_av ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary

data_H2 %>% 
  lmer(cep_ad ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary

data_H2 %>% 
  lmer(cep_fs ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = .) %>% 
  summary


### Explorative Untersuchungen
# 1. Betrachtung ausschließlich der Planungen, die wirklich verändert wurden

data_H2n <- data_H2[-c(3, 4, 13, 14, 25, 26, 31, 32, 35, 36, 39, 40, 55, 56, 77, 78, 79, 80, 83, 84, 91, 92, 95, 96, 99, 100, 101, 102, 103, 104, 105, 106),]
lmer(cep_ges ~ mzp + gruppe + mzp:gruppe + (1 | cod_stu), data = data_H2n) %>% 
  summary

# 2. Betrachtung der Daten gemeinsam mit den Planungen
#Daten im Long-Format
data_U1 = read.csv2('cache/Fragebögen und Tests.csv')

# Umpolen von negativen Items
data_U1$fb_st_ein <- 7 - data_U1$fb_st_ein 
data_U1$fb_ad_sch <- 7 - data_U1$fb_ad_sch

# Berechnung der relativen Punktzahl
data_U1$prä_rel <- data_U1$prä_ges / data_U1$prä_max
data_U1$post_rel <- data_U1$post_ges / data_U1$post_max

# Berechnung der relativen Verbesserung
data_U1$verbesserung <- data_U1$post_rel - data_U1$prä_rel

# Erstellen eines kompatiblen Datensatzes aus Untersuchung 2
data_TG1 = subset(data, gruppe == 0, select = c(cod_stu, mzp, cep_glo, cep_ges))

# Verbinden der beiden Datensätze
data_exp <- full_join(data_U1, data_TG1, by = c("cod_stu", "mzp"))

# Berechnung des Einflusses des Gesamturteils auf die Verbesserung
lmer(verbesserung ~ cep_ges + (1 | cod_stu), data = data_exp) %>% 
  summary

lmer(verbesserung ~ cep_glo + (1 | cod_stu), data = data_exp) %>% 
  summary
