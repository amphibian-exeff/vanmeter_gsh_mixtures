dim(rvm_abundance_xcms)
#View(rvm_abundance_xcms)

xcms_ncols <- ncol(rvm_abundance_xcms)

#2758, 15
rt_results_xcms <- data.frame(matrix(nrow = xcms_ncols, ncol = 19))
rt_colnames <- c("c_mean", "n_mean", "l_mean", "ln_mean", "z_mean", "zn_mean", "zl_mean", "zln_mean",
       "z_fvalue", "l_fvalue", "n_fvalue", "zl_fvalue", "zn_fvalue", "ln_fvalue", "zln_fvalue", 
       "significant05?", "significant01?", "main_significant05?", "main_significant01?")
colnames(rt_results_xcms) <- rt_colnames


for(i in 5:xcms_ncols){
  temp_aov <- bind_cols(rvm_abundance_xcms$Class, rvm_abundance_xcms$Z, rvm_abundance_xcms$L, rvm_abundance_xcms$N, log(rvm_abundance_xcms[,i]),
                          .name_repair = c("unique")
  )
  colnames(temp_aov) <- c("treatment", "Z", "L", "N", "log_abundance_xcms")
  #dim(temp_aov)
  #colnames(temp_aov)
  
  #class(temp_aov$treatment)
  #class(temp_aov$Z)
  
  temp_summary <- temp_aov %>%
    group_by(Z, L, N) %>%
    get_summary_stats(log_abundance_xcms, type = "mean_sd")
  
  rt_results_xcms[i-4,1:8] <- temp_summary$mean
  three_way_aov <- aov(log_abundance_xcms ~ Z * L * N, data = temp_aov)
  threewayaov_summary <- summary(three_way_aov)
  
  rt_results_xcms[i-4,9:15] <- threewayaov_summary[[1]][5]$`Pr(>F)`[1:7]
  print(i)
  
  #test for 0.05
  for(j in 1:7){
    if(threewayaov_summary[[1]][5]$`Pr(>F)`[j]<0.05){
      rt_results_xcms[i-4,16] <- TRUE
      break
    } 
    else {
      rt_results_xcms[i-4,16] <- FALSE
    }
  }
  print(rt_results_xcms[i-4,16])
  
  #test for 0.01
  for(j in 1:7){
    if(threewayaov_summary[[1]][5]$`Pr(>F)`[j]<0.01){
      rt_results_xcms[i-4,17] <- TRUE
      break
    } 
    else {
      rt_results_xcms[i-4,17] <- FALSE
    }
  }
  
  #test for 0.05
  for(j in 1:3){
    if(threewayaov_summary[[1]][5]$`Pr(>F)`[j]<0.05){
      rt_results_xcms[i-4,18] <- TRUE
      break
    } 
    else {
      rt_results_xcms[i-4,18] <- FALSE
    }
  }
  print(rt_results_xcms[i-4,18])
  
  #test for 0.01
  for(j in 1:3){
    if(threewayaov_summary[[1]][5]$`Pr(>F)`[j]<0.01){
      rt_results_xcms[i-4,19] <- TRUE
      break
    } 
    else {
      rt_results_xcms[i-4,19] <- FALSE
    }
  }
}

sum(rt_results_xcms$`significant05?`)
sum(rt_results_xcms$`significant01?`)
sum(rt_results_xcms$`main_significant05?`)
sum(rt_results_xcms$`main_significant01?`)

View(rt_results_xcms)
export_rt_results_xcms <- cbind(retention_time, rt_results_xcms)
View(export_rt_results_xcms)

rvm_group_stats_file <- paste(rvm_data_out,"/rvm_summary_stats_table_xcms.csv",sep="")
write.csv(export_rt_results_xcms, rvm_group_stats_file)

colnames(export_rt_results_xcms)


rvm_heatmap_data <- export_rt_results_xcms

p <- ggplot(m3,aes(x=year,y=state,fill=count))+
  geom_tile()
