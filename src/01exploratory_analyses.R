# 8 treatments with 6 samples each

dim(rvm_data)
colnames(rvm_data)
unique(rvm_data$Analyte)


ggplot(rvm_data, aes_string(x='treatment', y='GSH_nM_mL')) +
  geom_point() +
  xlab("Analyte") + ylab("Concentration") + ggtitle("GSH Levels") +
  theme_bw()

