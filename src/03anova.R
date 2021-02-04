# rvm_cort
# anova on gsh
# based on 3 main effects with the different stressors as between-group factors
# also looking for interaction effects
# no blocking variables present -- weight/surface area?
# completely randomized design where individual amphibians were assigned to different treatments

# check for outliers
# looking for outliers in each cell of the design
# https://www.datanovia.com/en/lessons/anova-in-r/
# three-way between-groups ANOVA

#### GSH
dim(rvm_cort)
colnames(rvm_cort)
summary(rvm_cort)

rvm_cort %>%
  group_by(Z, L, N) %>%
  get_summary_stats(GSH_nM_mL, type = "mean_sd")

# add logged GSH
rvm_cort$logGSH <- log(rvm_cort$GSH_nM_mL)

#################################
# identify outliers
rvm_cort %>%
  group_by(Z, L, N) %>%
  identify_outliers(GSH_nM_mL)

rvm_cort_extreme_outliers <- which(rvm_cort$ID=='C17')
rvm_cort_outliers <- which(rvm_cort$ID=='C17' | rvm_cort$ID=='LN1' | rvm_cort$ID=='Z18' | rvm_cort$ID=='ZLN13')

rvm_cort_drop_extreme_outliers <- rvm_cort[-rvm_cort_extreme_outliers,]
dim(rvm_cort_drop_extreme_outliers)

rvm_cort_drop_outliers <- rvm_cort[-rvm_cort_outliers,]
dim(rvm_cort_drop_outliers)

rvm_cort %>%
  group_by(Z, L, N) %>%
  identify_outliers(logGSH)

rvm_log_cort_outliers <- which(rvm_cort$ID=='C17' | rvm_cort$ID=='L16' | rvm_cort$ID=='LN17' | rvm_cort$ID=='ZN16')
rvm_log_cort_drop_outliers <- rvm_cort[-rvm_log_cort_outliers,]
dim(rvm_log_cort_drop_outliers)

#################################
# check normality
rvm_cort_lm  <- lm(GSH_nM_mL ~ Z*L*N, data = rvm_cort)
ggqqplot(residuals(rvm_cort_lm)) # Create a QQ plot of residuals
shapiro_test(residuals(rvm_cort_lm)) # Compute Shapiro-Wilk test of normality

#################################
#check normality by groups
rvm_cort %>%
  group_by(Z, L, N) %>%
  shapiro_test(GSH_nM_mL)
# (hard reject normality for 2 groups, soft reject for 1 group)

rvm_cort_drop_extreme_outliers %>%
  group_by(Z, L, N) %>%
  shapiro_test(GSH_nM_mL)
# (hard reject normality for 1 groups, soft reject for 1 group)

rvm_cort_drop_outliers %>%
  group_by(Z, L, N) %>%
  shapiro_test(GSH_nM_mL)
# (hard reject normality for 1 groups)

rvm_cort %>% #logged
  group_by(Z, L, N) %>%
  shapiro_test(logGSH)
# (soft reject normality for 2 groups)

rvm_log_cort_drop_outliers %>% #logged
  group_by(Z, L, N) %>%
  shapiro_test(logGSH)
# (soft reject normality for 2 groups)

## Conclusion: log data

#################################
#qqplot for each design cell (logged)
ggqqplot(rvm_cort, "logGSH", ggtheme = theme_bw()) +
  facet_grid(Z + L ~ N, labeller = "label_both")

ggqqplot(rvm_log_cort_drop_outliers, "logGSH", ggtheme = theme_bw()) +
  facet_grid(Z + L ~ N, labeller = "label_both")

#################################
#homogeneity of variance via Levene test
rvm_cort %>% levene_test(GSH_nM_mL ~ Z*L*N)
rvm_log_cort_drop_outliers %>% levene_test(logGSH ~ Z*L*N) #logged data with outliers dropped

##########################################
View(rvm_cort)

#################################
# aovs w and wo outliers
three_way_aov <- aov(GSH_nM_mL ~ Z * L * N, data = rvm_cort)
summary(three_way_aov)

three_way_aov_drop_outliers <- aov(GSH_nM_mL ~ Z*L*N, data = rvm_cort_drop_outliers)
summary(three_way_aov_drop_outliers)

three_way_aov_drop_extreme_outliers <- aov(GSH_nM_mL ~ Z*L*N, data = rvm_cort_drop_extreme_outliers)
summary(three_way_aov_drop_extreme_outliers)

#################################
# logged aovs w and wo outliers
logged_three_way_aov <- aov(logGSH ~ Z * L * N, data = rvm_cort)
summary(logged_three_way_aov)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Z            1   4.25   4.248   2.518 0.120432    
#L            1   0.60   0.598   0.354 0.555053    
#N            1  22.20  22.197  13.156 0.000802 ***
#  Z:L          1   6.63   6.626   3.927 0.054414 .  
#Z:N          1   4.52   4.518   2.678 0.109612    
#L:N          1   1.57   1.568   0.929 0.340892    
#Z:L:N        1   0.02   0.021   0.012 0.912750    
#Residuals   40  67.49   1.687 

## this is the aov we use, logged response data with appropriate outliers dropped
logged_three_way_aov_drop_outliers <- aov(logGSH ~ Z * L * N, data = rvm_log_cort_drop_outliers)
summary(logged_three_way_aov_drop_outliers)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Z            1   5.96   5.962   4.784 0.035308 *  
#  L            1   0.01   0.014   0.011 0.916558    
#N            1  22.99  22.988  18.445 0.000126 ***
#  Z:L          1  13.90  13.896  11.150 0.001965 ** 
#  Z:N          1   4.02   4.023   3.228 0.080773 .  
#L:N          1   4.03   4.027   3.231 0.080651 .  
#Z:L:N        1   0.31   0.314   0.252 0.618535    
#Residuals   36  44.87   1.246                     
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


