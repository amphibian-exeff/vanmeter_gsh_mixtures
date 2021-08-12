# these are 2-way anovas with a 2x2 matrix and peticide bcf as the response variable
# underpowered
# recommended sample size is 15 for power - 0.8, confidence level = 0.05, effect size = medium (0.75)
# https://www.kcl.ac.uk/health/research/specialistcentres/cib/resources/sample-size-and-power-analysis.pdf


############### atrazine ######################
dim(rvm_atrazine)
summary(rvm_atrazine)
View(rvm_atrazine)
# 4/24 >1
min(rvm_atrazine$BCF)
max(rvm_atrazine$BCF)
mean(rvm_atrazine$BCF)
sd(rvm_atrazine$BCF)

rvm_atrazine %>%
  group_by(L, N) %>%
  get_summary_stats(BCF, type = "mean_sd")
# A tibble: 4 x 6
#L     N     variable     n  mean    sd
#<fct> <fct> <chr>    <dbl> <dbl> <dbl>
#  1 No    No    BCF          6 0.492 0.562
#2 No    Yes   BCF          6 0.106 0.104
#3 Yes   No    BCF          6 0.698 0.934
#4 Yes   Yes   BCF          6 0.614 0.661

aov_atrazine_bcf <- aov(BCF ~ L * N, data = rvm_atrazine)
summary(aov_atrazine_bcf)

############### alachlor ######################
dim(rvm_alachlor)
summary(rvm_alachlor)
View(rvm_alachlor)
# 2/24 >1
min(rvm_alachlor$BCF)
max(rvm_alachlor$BCF)
mean(rvm_alachlor$BCF)
sd(rvm_alachlor$BCF)
rvm_alachlor %>%
  group_by(Z, N) %>%
  get_summary_stats(BCF, type = "mean_sd")
# A tibble: 4 x 6
#Z     N     variable     n  mean    sd
#<fct> <fct> <chr>    <dbl> <dbl> <dbl>
#  1 No    No    BCF          6 0.192 0.271
#2 No    Yes   BCF          6 0.136 0.077
#3 Yes   No    BCF          6 0.488 0.583
#4 Yes   Yes   BCF          6 0.482 0.6 

aov_alachlor_bcf <- aov(BCF ~ Z * N, data = rvm_alachlor)
summary(aov_alachlor_bcf)
