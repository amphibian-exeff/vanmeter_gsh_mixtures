#### GSH
dim(rvm_cort)
colnames(rvm_cort)
# "ID"        "Z"         "L"         "N"         "treatment" "GSH_nM_mL"

levels(rvm_cort$treatment)
# "C"   "L"   "LN"  "N"   "Z"   "ZL"  "ZLN" "ZN"

# before
ggplot(rvm_cort_before_sort, aes_string(x='treatment', y='GSH_nM_mL')) +
  geom_point() +
  xlab("Treatment") + ylab("Concentration") + ggtitle("GSH Levels") +
  theme_bw()
#update order
# "C"   "L"   "N"   "Z" "LN"     "ZL" "ZN" "ZLN" 
treatment_levels <- c("C", "Z", "L", "N", "ZL", "ZN", "LN", "ZLN")
#tidyverse arrange
rvm_cort$treatment <- factor(rvm_cort$treatment , levels=treatment_levels)
rvm_cort %>% arrange(treatment)
levels(rvm_cort$treatment)
# "C"   "L"   "N"   "Z"   "LN"  "ZL"  "ZN"  "ZLN"  
# after
ggplot(rvm_cort, aes_string(x='treatment', y='GSH_nM_mL')) +
  geom_point() +
  xlab("Treatment") + ylab("Concentration") + ggtitle("GSH Levels") +
  theme_bw()

# box plot on gsh levels
g_gsh_box <- ggplot(aes(y=GSH_nM_mL, x=treatment), data=rvm_cort) + 
  geom_boxplot(fill="darkolivegreen4") + 
  theme_bw() +
  labs(x = "Treatment", y="GSH (nM/mL)")
         
#expression(paste("GSH (", mu, "g/g), Water (", mu, "g/mL)", sep="")))
g_gsh_box

cort_boxplot <- paste(rvm_graphics,"/rvm_gsh_by_treatment.jpg",sep="")
jpeg(cort_boxplot, width = 8, height = 7, units = "in",res=600)
  g_gsh_box
dev.off()

log_g_gsh_box <- ggplot(aes(y=GSH_nM_mL, x=treatment), data=rvm_cort) + 
  geom_boxplot(fill="darkolivegreen4") + 
  theme_bw() +
  scale_y_continuous(trans='log') +
  labs(x = "Treatment", y="GSH (nM/mL)")
log_g_gsh_box

log_cort_boxplot <- paste(rvm_graphics,"/rvm_log_gsh_by_treatment.jpg",sep="")
jpeg(log_cort_boxplot, width = 8, height = 7, units = "in",res=600)
  log_g_gsh_box
dev.off()

### Atrazine
dim(rvm_atrazine)
colnames(rvm_atrazine)
# "ID"         "Z"          "L"          "N"          "treatment"  "Frog..ppm." "Soil..ppm." "BCF" 

levels(rvm_atrazine$treatment)
# "Z"   "ZL"  "ZLN" "ZN"

#update order
# "Z"   "ZL"  "ZN" "ZLN"
#tidyverse arrange
atrazine_treatment_levels <- c("Z", "ZL", "ZN", "ZLN")
rvm_atrazine$treatment <- factor(rvm_atrazine$treatment , levels=atrazine_treatment_levels)
rvm_atrazine %>% arrange(treatment)
levels(rvm_atrazine$treatment)
# "Z"   "ZL"  "ZN"  "ZLN"  

# box plot on gsh levels
g_atrazine_box <- ggplot(aes(y=BCF, x=treatment), data=rvm_atrazine) + 
  geom_boxplot(fill="cornflowerblue") + 
  theme_bw() +
  labs(x = "Treatment", y="Atrazine BCF")

#expression(paste("GSH (", mu, "g/g), Water (", mu, "g/mL)", sep="")))
g_atrazine_box

atrazine_boxplot <- paste(rvm_graphics,"/rvm_atrazine_bcf_by_treatment.jpg",sep="")
  jpeg(atrazine_boxplot, width = 8, height = 7, units = "in",res=600)
  g_atrazine_box
dev.off()


### Alachlor
dim(rvm_alachlor)
colnames(rvm_alachlor)
# "ID"         "Z"          "L"          "N"          "treatment"  "Frog..ppm." "Soil..ppm." "BCF" 

levels(rvm_alachlor$treatment)
# "C"   "L"   "LN"  "N"   "Z"   "ZL"  "ZLN" "ZN"

#order does not need to be updated
alachlor_treatment_levels <- c("L", "ZL", "LN", "ZLN")
rvm_alachlor$treatment <- factor(rvm_alachlor$treatment , levels=alachlor_treatment_levels)
rvm_alachlor %>% arrange(treatment)
levels(rvm_alachlor$treatment)

# box plot on gsh levels
g_alachlor_box <- ggplot(aes(y=BCF, x=treatment), data=rvm_alachlor) + 
  geom_boxplot(fill="red4") + 
  theme_bw() +
  labs(x = "Treatment", y="Alachlor BCF")

#expression(paste("GSH (", mu, "g/g), Water (", mu, "g/mL)", sep="")))
g_alachlor_box

alachlor_boxplot <- paste(rvm_graphics,"/rvm_alachlor_bcf_by_treatment.jpg",sep="")
  jpeg(alachlor_boxplot, width = 8, height = 7, units = "in",res=600)
  g_alachlor_box
dev.off()

# create single multiplot of all 3
#combine and stack figures
figure_stacked <- ggarrange(g_gsh_box, g_atrazine_box, g_alachlor_box,
                            heights = c(2.5, 2.5, 2.5),
                            labels = c("A", "B", "C"),
                            ncol = 1, nrow = 3)
figure_stacked

stacked_boxplots <- paste(rvm_graphics,"/rvm_cort_bcfs_stacked_figure.jpg",sep="")
jpeg(stacked_boxplots, width = 4, height = 7.5, units = "in",res=600)
  figure_stacked
dev.off()
