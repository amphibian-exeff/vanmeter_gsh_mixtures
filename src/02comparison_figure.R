dim(rvm_data)
colnames(rvm_data)
# "ID"        "Z"         "L"         "N"         "treatment" "GSH_nM_mL"

levels(rvm_data$treatment)
# "C"   "L"   "LN"  "N"   "Z"   "ZL"  "ZLN" "ZN"

# "C"   "L"   "N"   "Z" "LN"     "ZL" "ZN" "ZLN" 
levels(rvm_data$treatment) <- c("C", "L", "N", "Z", "LN", "ZL", "ZN", "ZLN")

rvm_data$treatment <- factor(rvm_data$treatment , levels=c("C", "L", "N", "Z", "LN", "ZL", "ZN", "ZLN"))

levels(rvm_data$treatment)
# "C"   "L"   "N"   "Z"   "LN"  "ZL"  "ZN"  "ZLN"  


# box plot on gsh levels
g_gsh_box <- ggplot(aes(y=GSH_nM_mL, x=treatment), data=rvm_data) + 
  geom_boxplot() + 
  theme_bw() +
  labs(x = "", y="GSH")
         
#expression(paste("GSH (", mu, "g/g), Water (", mu, "g/mL)", sep="")))
g_gsh_box

cort_boxplot <- paste(rvm_graphics,"/rvm_gsh_by_treatment.jpg",sep="")
jpeg(cort_boxplot, width = 8, height = 7, units = "in",res=600)
  g_gsh_box
dev.off()
