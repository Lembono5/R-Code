############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 4:                             ###
###                        BERECHNUNG HYPOTHESE 4                        ###
###                                                                      ###
############################################################################
############################################################################

# Hypothese 3: Der gemessene Kompetenzzuwachs der SchülerInnen korreliert positiv mit dem
# subjektiven Rating der Unterrichtsminiatur durch die Schüler*innen.

# Beschreibung der Ratings insgesamt
# Mittelwerte ausgeben lassen
subset(data[, c(103:111)]) %>% 
  colMeans(., na.rm = T)

# Definition einer Funktion, die die Standardabweichung ausgibt
colSd <- function (x, na.rm=T) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
subset(data[, c(103:111)]) %>% 
  colSd(.)

# Rechnen einer linear mixed effects regression zwischen prozentualem Punktezuwachs und den Kategorien, dem Gesamturteil bzw. der Globalnote
data %>% 
  lmer(verbesserung ~ fb_st + fb_ad + fb_se + fb_sy + fb_sw + fb_ma + fb_vi + fb_fs + (1 | cod_sus), data = .) %>% 
  summary

data %>% 
  lmer(verbesserung ~ ges + (1 | cod_sus), data = .) %>% 
  summary

data %>% 
  lmer(verbesserung ~ fb_glo + (1 | cod_sus), data = .) %>% 
  summary

# Explorative Untersuchungen
# Berechnung der durchschnittlichen Bewertungen nach MZP und mögliche Unterschiede
describeBy(data$ges, data$mzp)
leveneTest(data$ges, data$mzp)
t.test(data$ges ~ data$mzp, var.equal = T, alternative = "two.sided")
cohensD(data$ges, data$mzp)

# Berechnung des Zusammenhangs der Verbesserung mit den Gesamturteilen der Kategorien nach MZP
data %>% 
  lmer(verbesserung ~ mzp + fb_glo : mzp + fb_st : mzp + fb_ad : mzp + 
         fb_se : mzp +
         (1 | cod_sus) + (1 | cod_stu), data = .) %>% 
  summary

# Berechnung der Abhängigkeit des Globalurteils von den einzelnen Konstrukten
data %>% 
  lmer(fb_glo ~ fb_st + fb_ad + fb_se + fb_sy + fb_sw + fb_ma + fb_vi + fb_fs + (1 | cod_sus), data = .) %>% 
  summary

# Berechnung der Abhängigkeit des Globalurteils von den einzelnen Konstrukten im Vergleich zu FALKE-q
data %>% 
  lmer(fb_glo ~ fb_st + fb_ad + fb_se + fb_sw + (1 | cod_sus), data = .) %>% 
  summary


data %>% 
  mutate(
    Messzeitpunkt = factor(if_else(mzp == 1, 'prä_rel', 'post_rel'), levels = c('prä_rel', 'post_rel'))
  ) %>% 
  lmer(verbesserung ~ mzp + fb_glo : mzp + fb_st : mzp + fb_ad : mzp + 
         fb_se : mzp + fb_sy : mzp + fb_sw : mzp + 
         (1 | cod_sus) + (1 | cod_stu), data = .) %>% 
  summary

# Untersuchung der Bewertung ausschließlich mit Schüler*innen, denen das Thema zuvor unbekannt war
data2 <- subset(data, data$fb_fs_bvw > 3)
describeBy(data2$ges, data2$mzp)
leveneTest(data2$ges, data2$mzp)
t.test(data2$ges ~ data2$mzp, var.equal = T, alternative = "two.sided")
cohensD(data2$ges, data2$mzp)


# Untersuchung der Verbesserung ausschließlich mit Schüler*innen, denen das Thema zuvor unbekannt war
describeBy(data2$verbesserung, data2$mzp)
lmer(verbesserung ~ fb_st + fb_ad + fb_se + fb_sy + fb_sw + fb_ma + fb_vi + fb_fs + (1 | cod_sus), data = data2) %>% 
  summary
lmer(verbesserung ~ ges + (1 | cod_sus), data = data2) %>% 
  summary

# Berechnung, wie groß der Effekt hätte sein müssen
library(simr)
model <- lmer(verbesserung ~ ges + (1 | cod_sus), data = data2)

model_large <- model
fixef(model_large)["ges"] <- -0.05
model_large
powerSim(model_large, nsim = 100, test = fcompare(verbesserung ~ ges))
