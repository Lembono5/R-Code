###########################################################################
###########################################################################
###                                                                     ###
###                            ABSCHNITT 1:                             ###
###                     BERECHNUNG ALLGEMEINE WERTE                     ###
###                                                                     ###
###########################################################################
###########################################################################

# Umpolen von negativen Items
data$fb_st_ein <- 7 - data$fb_st_ein 
data$fb_ad_sch <- 7 - data$fb_ad_sch

# Berechnung der relativen Punktzahl
data$prä_rel <- data$prä_ges / data$prä_max
data$post_rel <- data$post_ges / data$post_max

# Berechnung der relativen Verbesserung
data$verbesserung <- data$post_rel - data$prä_rel

# Erstellen neuer Variablen als Durchschnittswerte für die Konstrukte
data$fb_st <- rowSums(data[, c(21:24)], na.rm = F) / 4
data$fb_ad <- rowSums(data[, c(25, 26)], na.rm = F) / 2
data$fb_se <- rowSums(data[, c(27:32)], na.rm = F) / 6
data$fb_sw <- data$fb_sw_ver
data$fb_sy <- rowSums(data[, c(34:36)], na.rm = F) / 3                      
data$fb_ma <- rowSums(data[, c(37:41)], na.rm = F) / 5
data$fb_vi <- rowSums(data[, c(42:45)], na.rm = F) / 4
data$fb_fs <- rowSums(data[, c(46:77)], na.rm = T) / 8
data$ges <- rowSums(data[,c(21:77)], na.rm = T) / 33


### Berechnung von Cronbachs Alphas/Spearman-Brown für interne Konsistenz
# Strukturiertheit
alpha(subset(data, select = c(fb_st_was, fb_st_rot, fb_st_bau, fb_st_ein)), check.keys = T)

# Adressat*innenorientierung (2 Items, deshalb Spearman-Brown)
cor.test(data$fb_ad_sch, data$fb_ad_vwi, method="spearman")

# Sprech- und Körperausdruck
alpha(subset(data, select = c(fb_se_sti, fb_se_aus, fb_se_ges, fb_se_pau, fb_se_abw, fb_se_kor)), check.keys = T)

# Persönlichkeitswirkung
alpha(subset(data, select = c(fb_sy_sym, fb_sy_beg, fb_sy_nat)), check.keys = T)

# Adaptivität
alpha(subset(data, select = c(fb_ma_ant, fb_ma_ver, fb_ma_rea, fb_ma_ged, fb_ma_wei)), check.keys = T)

# Visualisierung
alpha(subset(data, select = c(fb_vi_dar, fb_vi_ube, fb_vi_ver, fb_vi_eri)), check.keys = T)

# Fachspezifika
alpha(subset(data, select = c(fb_fs_obf, fb_fs_sal, fb_fs_agp, fb_fs_dsb, fb_fs_saf)), check.keys = T)
