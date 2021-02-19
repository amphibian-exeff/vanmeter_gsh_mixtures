dim(rvm_abundance)

#2793, 15
rt_results <- df <- data.frame(matrix(nrow = 2793, ncol = 16))
rt_colnames <- c("c_mean", "n_mean", "l_mean", "ln_mean", "z_mean", "zn_mean", "zl_mean", "zln_mean",
       "z_fvalue", "l_fvalue", "n_fvalue", "zl_fvalue", "zn_fvalue", "ln_fvalue", "zln_fvalue", "significant?")
colnames(rt_results) <- rt_colnames


for(i in 5:2797){
  temp_aov <- bind_cols(rvm_abundance$Class, rvm_abundance$Z, rvm_abundance$L, rvm_abundance$N, log(rvm_abundance[,i]),
                          .name_repair = c("unique")
  )
  colnames(temp_aov) <- c("treatment", "Z", "L", "N", "log_abundance")
  #dim(temp_aov)
  #colnames(temp_aov)
  
  #class(temp_aov$treatment)
  #class(temp_aov$Z)
  
  temp_summary <- temp_aov %>%
    group_by(Z, L, N) %>%
    get_summary_stats(log_abundance, type = "mean_sd")
  
  rt_results[i-4,1:8] <- temp_summary$mean
  three_way_aov <- aov(log_abundance ~ Z * L * N, data = temp_aov)
  threewayaov_summary <- summary(three_way_aov)
  
  rt_results[i-4,9:15] <- threewayaov_summary[[1]][5]$`Pr(>F)`[1:7]
  print(i)
  
  for(j in 1:7){
    if(threewayaov_summary[[1]][5]$`Pr(>F)`[j]<0.05){
      rt_results[i-4,16] <- TRUE
      break
    } 
    else {
      rt_results[i-4,16] <- FALSE
    }
  }
  print(rt_results[i-4,16])
}

sum(rt_results$`significant?`)

rvm_group_stats_file <- paste(rvm_data_out,"/rvm_summary_stats_table.csv",sep="")
write.csv(rt_results, rvm_group_stats_file)
