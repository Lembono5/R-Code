############################################################################
############################################################################
###                                                                      ###
###                             ABSCHNITT 4:                             ###
###                         EXPLORATIVE ANALYSEN                         ###
###                                                                      ###
############################################################################
############################################################################

# Post-Hoc Poweranalyse

data_H3 <- subset(data, gruppe == 0, select = c(cod_stu, mzp, cep_ges))

data_2 = read.csv2('cache/Fragebögen und Tests.csv')
data_2$prä_rel <- data_2$prä_ges / data_2$prä_max
data_2$post_rel <- data_2$post_ges / data_2$post_max
data_2$verbesserung <- data_2$post_rel - data_2$prä_rel

data_H3n <- full_join(data_2, data_H3, by = c("cod_stu", "mzp"))

lmer(verbesserung ~ cep_ges + (1 | cod_stu) + (1|cod_sus), data = data_H3n) %>% 
  summary
