############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 3:                             ###
###                        BERECHNUNG HYPOTHESE 3                        ###
###                                                                      ###
############################################################################
############################################################################

# Hypothese 3: Die erneute Durchführung einer Unterrichtsminiatur im Fach
# Sport nach videobasierter Reflexion und Überarbeitung der Erklärung führt zu
# einer signifikant höheren Bewertung durch die Zielgruppe.

### Deskriptive Statistik
# Noten zu MZP 1
subset(data, mzp == 1, select = c("fb_glo")) %>%
  table

# Noten zu MZP 2
subset(data, mzp == 2, select = c("fb_glo")) %>%
  table

### Inferenzstatistische Analyse
# Berechnung mit einem einseitigen t-Test für unabhängige Stichporben mithilfe der 
# durchschnittlichen Verbesserung von Prä- auf Posttest
describeBy(data$fb_glo, data$mzp)

# Zuvor eine Levene Test durchführen:
leveneTest(data$fb_glo, data$mzp)

# Daten clustern, nach MZP'en in zwei Gruppen aufteilen und anschließend Verbesserungen vergleichen
t.test(data$fb_glo ~ data$mzp, var.equal = T, alternative = "greater")
cohensD(data$fb_glo, data$mzp)

### Grafiken zu Noten im Prä- und Posttest
subset(data, mzp == 1, select = c("fb_glo")) %>% 
  summary

note_1 <- subset(data, mzp == 1, select = c("fb_glo")) %>%
  table %>% 
  as.data.frame

note_1$Prozent <- paste(round(note_1$Freq/sum(note_1$Freq)*100,2), "%")
note_1$Prozent[3:6] <- NA

plot_1 <- ggplot(note_1, aes(x = "", y = Freq, fill = fb_glo)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y") +
  theme_void() +
  scale_fill_manual(values = c("darkblue", "darkgreen", "yellow", "orange", "red", "darkred")) +
  geom_label(aes(label = Prozent),
             position = position_stack(vjust = 0.5),
             color=c("white", "white", "black", "black", "black", "black"),
             label.size = 0,
             size = 6, 
             show.legend = FALSE) +
  labs(fill = "Note") +
  theme(legend.position="none")
plot_1


subset(data, mzp == 2, select = c("fb_glo")) %>% 
  summary

note_2 <- subset(data, mzp == 2, select = c("fb_glo")) %>% 
  table %>% 
  as.data.frame

note_2$Prozent <- paste(round(note_2$Freq/sum(note_2$Freq)*100,2), "%")
note_2$Prozent[3:6] <- NA

plot_2 <- ggplot(note_2, aes(x = "", y = Freq, fill = fb_glo)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y") +
  theme_void() +
  scale_fill_manual(values = c("darkblue", "darkgreen", "yellow", "orange", "red", "darkred")) +
  geom_label(aes(label = Prozent),
             position = position_stack(vjust = 0.5),
             color=c("white", "white", "black", "black", "black", "black"),
             label.size = 0,
             size = 6, 
             show.legend = FALSE) +
  labs(fill = "Note") +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=15))
plot_2
