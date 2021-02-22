colnames(export_rt_results)

sum(export_rt_results$`significant01?`)
sig01_rows <- which(export_rt_results$`significant01?`==TRUE)

#na all concs with not significant aov F values
l_na_these <- which(export_rt_results$l_fvalue>0.05)
n_na_these <- which(export_rt_results$n_fvalue>0.05)
z_na_these <-  which(export_rt_results$z_fvalue>0.05)
ln_na_these <- which(export_rt_results$ln_fvalue>0.05)
zn_na_these <- which(export_rt_results$zn_fvalue>0.05)
zl_na_these <- which(export_rt_results$zl_fvalue>0.05)
zln_na_these <- which(export_rt_results$zln_fvalue>0.05)

l_na_these
n_na_these
z_na_these
ln_na_these
zn_na_these
zl_na_these
zln_na_these

#assign NAs to values for not significant F values
export_rt_results$l_mean[l_na_these] <- NA
export_rt_results$n_mean[n_na_these] <- NA
export_rt_results$z_mean[z_na_these] <- NA
export_rt_results$ln_mean[ln_na_these] <- NA
export_rt_results$zn_mean[zn_na_these] <- NA
export_rt_results$zl_mean[zl_na_these] <- NA
export_rt_results$zln_mean[zln_na_these] <- NA

# convert from wide to long format for heat map
rvm_heatmap_data_long <- gather(export_rt_results[sig01_rows,], key=treatment, value=logabundance, c_mean:zln_mean, factor_key=TRUE)
dim(rvm_heatmap_data_long)
colnames(rvm_heatmap_data_long)
View(rvm_heatmap_data_long)
levels(rvm_heatmap_data_long$treatment)
rvm_heatmap_data_long$logabundance


rvm_heatmap_w_cuts <- rvm_heatmap_data_long %>%
  mutate(logfactor=cut(logabundance,breaks=c(7,8,9,10,11),
                         labels=c("7-8","8-9","9-10",">10")))
  
heatmap_jpg <- ggplot(rvm_heatmap_w_cuts,aes(x=treatment,y=retention_time,fill=logfactor))+
  geom_tile() +
  geom_tile(colour="white",size=0.25) + # add white border
  guides(fill=guide_legend(title="Log\nAbundance"))+
  labs(x="",y="",title="Log Abundance of Significant Metabolite Treatments")+
  #scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0),
                 labels=c("C","N","L","LN","Z","ZN","ZL","ZLN")) +
  scale_fill_manual(values=rev(brewer.pal(4,"YlGnBu")),na.value="grey90")+
  theme_grey(base_size=8) + # control font size
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())

heatmap_jpg  

heatmap_jpg_filename <- paste(rvm_graphics,"/rvm_heatmap_significant_logabundance.jpg",sep="")
jpeg(heatmap_jpg_filename, width = 4, height = 6, units = "in",res=600)
  heatmap_jpg
dev.off()
