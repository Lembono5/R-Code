###########################################################################
###########################################################################
###                                                                     ###
###                            ABSCHNITT 2:                             ###
###                    BERECHNUNG HYPOTHESEN 1 UND 2                    ###
###                                                                     ###
###########################################################################
###########################################################################

# Hypothese 1: Die Durchführung einer Unterrichtsminiatur im Fach Sport führt zu 
# einem Kompetenzzuwachs aufseiten der teilnehmenden Schüler*innen.
# Berechnung mit einem einseitigen t-Test für abhängige Stichproben mithilfe der 
# relativen Punktzahlen im Prä- und Posttest
# Rechnen eines einseitigen t-Tests zu den relativen Punktzahlen im Prä- und Posttest
describe(data$prä_rel)
describe(data$post_rel)
t.test(data$prä_rel,data$post_rel, paired = TRUE, alternative = "less")

# Berechnung der Effektstärke
cohensD(data$prä_rel, data$post_rel, method ="paired")

# Grafiken
dat_univ = make.univ(data, dvs = cbind(data$prä_rel, data$post_rel), tname = 'MZP', outname = 'Punkte')
dat_univ %>% 
  mutate(
    Messzeitpunkt = factor(if_else(MZP == 0, 'Prätest', 'Posttest'), levels = c('Prätest', 'Posttest'))
  ) %>% 
  group_by(Messzeitpunkt) %>% 
  summarise(
    Punktzahl = mean(Punkte, na.rm = T),
    SE = sd(Punkte, na.rm = T)/sqrt(n())
  ) %>% 
  ggplot(aes(x = Messzeitpunkt, 
             y = Punktzahl, 
             ymax = Punktzahl + SE, 
             ymin = Punktzahl - SE,
             group = '1'
  )) +
  stat_summary(fun = identity, geom = 'line', show.legend = T) + 
  geom_point(show.legend = T) + geom_errorbar(width = .2) +
  geom_text(data = . %>% filter(Messzeitpunkt == "Prätest"),
            aes(label = round(Punktzahl, 2), y = Punktzahl),
            position = position_nudge(x = -0.2), size = 4, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = . %>% filter(Messzeitpunkt == "Posttest"),
            aes(label = round(Punktzahl, 2), y = Punktzahl),
            position = position_nudge(x = 0.2), size = 4, na.rm = TRUE, show.legend = FALSE) +
  ylim(c(0, 1)) +
  ylab("Prozentuale Punktzahl") +
  theme(
    text = element_text(size = 10),
    )


# Hypothese 2: Die erneute Durchführung einer Unterrichtsminiatur im Fach
# Sport nach videobasierter Reflexion und Überarbeitung der Erklärung führt zu
# einem signifikant höherem Kompetenzzuwachs bei der Zielgruppe als die erste
# Durchführung.
# Berechnung mit einem einseitigen t-Test für unabhängige Stichproben mithilfe der 
# durchschnittlichen Verbesserung von Prä- auf Posttest.
describeBy(data$verbesserung, data$mzp)
describeBy(data$prä_rel, data$mzp)
describeBy(data$post_rel, data$mzp)

# Zuvor eine Levene Test durchführen:
leveneTest(data$verbesserung, data$mzp)

# Daten clustern, nach MZP'en in zwei Gruppen aufteilen und anschließend Verbesserungen vergleichen
t.test(data$verbesserung ~ data$mzp, var.equal = T, alternative = "less")
cohensD(data$verbesserung, data$mzp)

# Grafiken
dat_univ %>% 
  mutate(
    Test = factor(if_else(MZP == 0, 'Prätest', 'Posttest'), levels = c('Prätest', 'Posttest')),
    MZP = factor(if_else(mzp == 1, 'MZP 1', 'MZP 2'), levels = c('MZP 1', 'MZP 2'))                     
  ) %>% 
  group_by(MZP, Test) %>% 
  summarise(
    Punktzahl = mean(Punkte, na.rm = T),
    SE = sd(Punkte, na.rm = T)/sqrt(n())
  ) %>% 
  ggplot(aes(x = MZP, y = Punktzahl, fill = Test)) +
  geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.8)) +
  geom_text(data = . %>% filter(Test == "Prätest"),
            aes(label = round(Punktzahl, 2), y = Punktzahl),
            position = position_nudge(x = -0.2), size = 4, na.rm = TRUE, show.legend = FALSE) +
  geom_text(data = . %>% filter(Test == "Posttest"),
            aes(label = round(Punktzahl, 2), y = Punktzahl),
            position = position_nudge(x = 0.2), size = 4, na.rm = TRUE, show.legend = FALSE)  +
  ylim(0, 1) +
  theme(
    legend.position="bottom",
    text = element_text(size = 10)
  )


# Explorative Untersuchungen
# Berechnen von Hypothese 1 ausschließlich mit Schüler*innen, denen das Thema nicht bekannt ist
data_n <- subset(data, data$fb_fs_bvw > 3)
t.test(data_n$prä_rel,data_n$post_rel, paired = TRUE, alternative = "less")
cohensD(data_n$prä_rel, data_n$post_rel, method ="paired")

# Berechnen von Hypothese 2 ausschließlich mit Schüler*innen, denen das Thema nicht bekannt ist
describeBy(data_n$verbesserung, data_n$mzp)
leveneTest(data_n$verbesserung, data_n$mzp)
t.test(data_n$verbesserung ~ data_n$mzp, var.equal = T, alternative = "less")
cohensD(data_n$verbesserung, data_n$mzp)
