###########################################################################
###########################################################################
###                                                                     ###
###                            ABSCHNITT 1:                             ###
###                     BERECHNUNG ALLGEMEINE WERTE                     ###
###                                                                     ###
###########################################################################
###########################################################################

data <- subset(data, gruppe == 0 | gruppe == 1 | gruppe == 2)

# Erstellen neuer Variablen
data$cke_st <- data[, c(22, 23, 24, 25, 26, 29, 30, 31, 34, 35)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cke_st_was + data$cke_st_bau + data$cke_st_rot + data$cke_st_ein + data$cke_st_end) / 5
data$cke_sy <- data[, c(22, 23, 24, 25, 26, 29, 30, 31, 34, 35)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cke_sy_beg + data$cke_sy_nat + data$cke_sy_sym) / 3
data$cke_ad <- data[, c(22, 23, 24, 25, 26, 29, 30, 31, 34, 35)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cke_ad_akt + data$cke_ad_did) / 2
data$cke_vi <- scale(data$cke_vi_vis)
data$cke_fs <- data[, c(34, 35)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cke_fs_vth + data$cke_fs_ric) / 2
data$cke_se <- data[, c(37, 38, 39, 40, 41, 42)] %>% 
  mutate_all(~(scale(.) %>% as.vector)) %>% 
  rowSums(data$cke_se_sti + data$cke_se_aus + data$cke_se_ges + data$cke_se_pau + data$cke_se_abw + data$cke_se_kor) / 6
data$cke_ges <- rowSums(data$cke_st + data$cke_sy + data$cke_ad + data$cke_vi + data$cke_se + data$cke_fs) / 6

# Berechnung des Einflusses der einzelnen Variablen auf die Globalnote
data %>% 
  lmer(cke_glo ~ cke_st + cke_ad + cke_vi + cke_sy + cke_fs + cke_se + (1 | cod_stu), data = .) %>% 
  summary  

