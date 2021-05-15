rvm_peak_ids_file <- paste(rvm_csv_in,"/initial_peak_ids_xcms.csv",sep="")

id_results <- read.csv(rvm_peak_ids_file)
#View(id_results)
dim(id_results)
colnames(id_results)
id_results$X

id_list <- rle(id_results$ID)$values
id_list_lengths <- rle(id_results$ID)$lengths
id_list
id_list_lengths

#plot retention_time v 
#c_mean	n_mean	l_mean	ln_mean	z_mean	zn_mean	zl_mean	zln_mean

#View(rvm_abundance_xcms_sample_names)
dim(rvm_abundance_xcms_sample_names)
rvm_abundance_xcms_sample_names$X <- 1:2956
colnames(rvm_abundance_xcms_sample_names)
  
#merge ided list with abundance on X
#right outer join
merged_ided_abundance <- merge(x =rvm_abundance_xcms_sample_names, y = id_results, by = "X", all.y = TRUE)
dim(merged_ided_abundance)
#convert retention time and abundances from factor to numeric
#lost some precision!
for(i in 2:48){
  merged_ided_abundance[,i] <- as.numeric(merged_ided_abundance[,i])
}
summary(merged_ided_abundance)

rt_pdf_filename <- paste(rvm_graphics,"/rvm_rt_v_abundance_significant_ids_xcms.pdf",sep="")
pdf(rt_pdf_filename, width = 8.5, height = 11, onefile = TRUE)
par(mfrow=c(1,1))
  id_begin <- 1
  counter <- 1
  for(id in id_list){
    print(id)
    id_end <- id_begin + id_list_lengths[counter] - 1
    get_rows <- id_begin:id_end #which(id_results$ID==id)
    id_begin <- id_end + 1
    counter <- counter + 1
    #only run if more than one element
    if(length(get_rows)>1){
      ### print the abundances
      id_abundance_subset <- merged_ided_abundance[get_rows,]
      #View(id_abundance_subset)
      #summary(id_abundance_subset)
      control_plot <- ggplot(id_abundance_subset, aes(x=X), color = "black") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = CON.13, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = CON.14, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = CON.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = CON.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = CON.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = CON.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": Control")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
                             axis.text.x=element_blank(),
                             axis.ticks.x=element_blank())
      
      l_plot <- ggplot(id_abundance_subset, aes(x=X), color = "blue2") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = L.13, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = L.14, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = L.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = L.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = L.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = L.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": L")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      n_plot <- ggplot(id_abundance_subset, aes(x=X), color = "darkred") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = N.13, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = N.14, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = N.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = N.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = N.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = N.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": N")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      z_plot <- ggplot(id_abundance_subset, aes(x=X), color = "darkgreen") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = Z.13, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = Z.14, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = Z.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = Z.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = Z.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = Z.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": Z")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())      
      
      ln_plot <- ggplot(id_abundance_subset, aes(x=X), color = "chartreuse2") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = LN.13, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = LN.14, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = LN.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = LN.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = LN.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = LN.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": LN")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      zl_plot <- ggplot(id_abundance_subset, aes(x=X), color = "cornflowerblue") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZL.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZL.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZL.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZL.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": ZL")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      zn_plot <- ggplot(id_abundance_subset, aes(x=X), color = "darkgoldenrod2") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZN.13, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZN.14, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZN.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZN.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZN.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZN.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": ZN")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      zln_plot <- ggplot(id_abundance_subset, aes(x=X), color = "darkmagenta") + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZLN.13, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZLN.14, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZLN.15, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZLN.16, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZLN.17, x=Sample),size=1.1, se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ZLN.18, x=Sample),size=1.1, se=F) +
        labs(title = id) +
        ggtitle(paste(id, ": ZLN")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic()
      
      ###plot the mean log abundances
      id_results_subset <- id_results[get_rows,]
      #View(id_results_subset)
      #plotting X instead of retention_time because not enough precision in retention_time
      rt_plot <- ggplot(id_results_subset, aes(x=X)) + 
        #ggtitle("title") +
        geom_smooth(method='loess', formula = y ~ x, aes(y = c_mean, x=X, color = "c_mean"),size=1.5, se=F) + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = n_mean, x=X, color="n_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = l_mean, x=X, color="l_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = z_mean, x=X, color="z_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ln_mean, x=X, color="ln_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = zn_mean, x=X, color="zn_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = zl_mean, x=X, color="zl_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = zln_mean, x=X, color="zln_mean"), se=F) +
        scale_color_manual(values = c(
                  "c_mean" = "black", 
                  "n_mean"="darkred", 
                  "l_mean"="blue2", 
                  "z_mean"="darkgreen",
                  "ln_mean"="chartreuse2", 
                  "zn_mean"="darkgoldenrod2", 
                  "zl_mean"="cornflowerblue", 
                  "zln_mean"="darkmagenta")) +
        labs(title = id) +
        xlab("bin (not retention time)") +
        ylab("abundance") +
        #ggtitle(id) #+
        theme_classic()
      
      #plot(rt_plot)
      #ggsave(filename = paste(rvm_graphics, "/rt_", id, "_image.png", sep=""))
      
      colnames(id_results_subset)
      rt_table <- id_results_subset %>%
        dplyr::select(-ID, -c_mean, -n_mean, -l_mean, -ln_mean, -z_mean,
                      -zn_mean, -zl_mean, -zln_mean,
                      -significant05., -significant01., -main_significant05., -main_significant01.) %>%
        gt() %>%
        tab_header(
          title = id
        ) %>%
        fmt_number(
          columns = vars(z_fvalue, l_fvalue, n_fvalue,
                         zl_fvalue, zn_fvalue, ln_fvalue,
                         zln_fvalue)
          #columns = vars(z_fvalue.1, l_fvalue.1, n_fvalue.1,
          #               zl_fvalue.1, zn_fvalue.1, ln_fvalue.1,
          #               zln_fvalue.1)
        )
      
      gtsave(rt_table, filename = paste(rvm_graphics, "/rt_", id, id_begin, "_table.png", sep=""))
      #boxc <- list(rt_plot, as_grob(rt_table))
      #ggarrange(plotlist=boxc, nrow = 2)  
      #grid.arrange(rt_plot, rt_table, nrow = 2)
      
      #tmp <- tempfile(fileext = '.png') #generate path to temp .png file
      #gtsave(rt_table, tmp) #save gt table as png
      png_table_filename = paste(rvm_graphics, "/rt_", id, id_begin, "_table.png", sep="")
      table_png <- png::readPNG(png_table_filename, native = TRUE) # read tmp png file
      
      #using patchwork library
      abundance_combined <-  (rt_plot / table_png) | (control_plot / l_plot / n_plot / z_plot / zl_plot / zn_plot / ln_plot / zln_plot)
      plot(abundance_combined)
    }
    #ggsave(filename = paste(rvm_graphics, "/rt_", id, "_combined.png", sep=""))
  }
dev.off()

