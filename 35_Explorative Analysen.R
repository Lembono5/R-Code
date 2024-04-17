############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 5:                             ###
###                         EXPLORATIVE ANALYSEN                         ###
###                                                                      ###
############################################################################
############################################################################

# Post-Hoc Poweranalyse für Hypothese 2
# Laden des Pakets
library(simr)

# Vorbereiten der Daten
data_sens <- subset(data, gruppe == 0 | gruppe == 1 | gruppe == 2, select = c(cod_stu, gruppe, mzp, cke_glo, cke_ges)) %>% 
  mutate(status = ifelse(gruppe < 2, "Treatment", "Kontrolle"))
lmer(cke_glo ~ mzp + status + mzp:status + (1 | cod_stu), data = data_sens) %>% 
  summary
lmer(cke_ges ~ mzp + status + mzp:status + (1 | cod_stu), data = data_sens) %>% 
  summary

# Poweranalyse für die Globalnote
model_glo <- lmer(cke_glo ~ mzp + status + mzp:status + (1 | cod_stu), data = data_sens)

model_glo_large <- model_glo
fixef(model_glo_large)["mzp:statusTreatment"] <- -0.6
powerSim(model_glo_large, nsim = 100, test = fcompare(cke_glo~status))

# Poweranalyse für das Gesamturteil
model_ges <- lmer(cke_ges ~ mzp + status + mzp:status + (1 | cod_stu), data = data_sens)

model_ges_large <- model_ges
fixef(model_ges_large)["mzp:statusTreatment"] <- .25
powerSim(model_ges_large, nsim = 100, test = fcompare(cke_ges~status))

