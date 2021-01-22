# 8 treatments with 6 samples each

dim(rvm_cort)
colnames(rvm_cort)
unique(rvm_cort$treatment)

ggplot(rvm_cort, aes_string(x='treatment', y='GSH_nM_mL')) +
  geom_point() +
  xlab("Treatment") + ylab("Concentration") + ggtitle("GSH Levels") +
  theme_bw()


dim(rvm_atrazine)
colnames(rvm_atrazine)
unique(rvm_atrazine$treatment)

ggplot(rvm_atrazine, aes_string(x='treatment', y='BCF')) +
  geom_point() +
  xlab("Treatment") + ylab("Concentration") + ggtitle("Atrazine BCFs") +
  theme_bw()



dim(rvm_alachlor)
colnames(rvm_alachlor)
unique(rvm_alachlor$treatment)

ggplot(rvm_alachlor, aes_string(x='treatment', y='BCF')) +
  geom_point() +
  xlab("Treatment") + ylab("Concentration") + ggtitle("Alachlor BCFs") +
  theme_bw()
