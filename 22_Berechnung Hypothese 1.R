############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 2:                             ###
###                        BERECHNUNG HYPOTHESE 1                        ###
###                                                                      ###
############################################################################
############################################################################

# Hypothese 1: Die Unterrichtsplanungen der beiden Gruppen unterscheiden sich 
# vor der Intervention nicht in ihrer Qualität.

data_H1 <- subset(data, mzp == 1 & gruppe < 2, select = c(gruppe, cep_ges))

# Berechnung mit einem zweiseitigen t-Test für unabhängige Stichproben mithilfe der 
# durchschnittlichen Bewertungen der beiden Gruppen zu MZP 1
describeBy(data_H1$cep_ges, data_H1$gruppe)

# Zuvor eine Levene Test durchführen:
leveneTest(data_H1$cep_ges, data_H1$gruppe)

# Daten clustern, nach MZP'en in zwei Gruppen aufteilen und anschließend Verbesserungen vergleichen
t.test(data_H1$cep_ges ~ data_H1$gruppe, var.equal = T)
cohensD(data_H1$cep_ges, data_H1$gruppe)
