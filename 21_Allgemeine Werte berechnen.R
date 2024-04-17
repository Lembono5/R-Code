###########################################################################
###########################################################################
###                                                                     ###
###                            ABSCHNITT 1:                             ###
###                       ALLGEMEINE BERECHNUNGEN                       ###
###                                                                     ###
###########################################################################
###########################################################################

# Erstellen neuer Variablen
data$cep_st <- data[, c(12, 13, 14, 15, 16)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cep_st_was + data$cep_st_bau + data$cep_st_rot + data$cep_st_ein + data$cep_st_end) / 5
data$cep_av <- data[, c(17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cep_av_ld1 + data$cep_av_qu + data$cep_av_pf + data$cep_av_sa) / 4
data$cep_ad <- data[, c(22, 23, 24, 25, 26, 29, 30, 31, 34, 35)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cep_ad_bei + data$cep_ad_akt + data$cep_ad_did) / 3
data$cep_fs <- data[, c(36, 37)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cep_fs_ric + data$cep_fs_vth) / 2
data$cep_ges <- data$cep_st + data$cep_av + data$cep_ad * data$cep_fs
            
# Berechnung des Einflusses der einzelnen Variablen auf die Gesamturteile
subset(data, mzp == 1) %>% 
  lmer(cep_glo ~ cep_st + cep_av + cep_ad + cep_fs + (1 | gruppe), data = .) %>% 
  summary  

subset(data, mzp == 2) %>% 
  lmer(cep_glo ~ cep_st + cep_av + cep_ad + cep_fs + (1 | gruppe), data = .) %>% 
  summary

lmer(cep_glo ~ cep_st + cep_av + cep_ad + cep_fs + (1 | gruppe), data = data) %>% 
  summary  

lmer(cep_ges ~ cep_st + cep_av + cep_ad + cep_fs + (1 | gruppe), data = data) %>% 
  summary 
