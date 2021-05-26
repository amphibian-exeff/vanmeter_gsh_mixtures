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
colnames(rvm_abundance_xcms_sample_names)
  
#merge ided list with abundance on X
#right outer join
#View(id_results)
#View(rvm_abundance_xcms_sample_names)
dim(id_results)
merged_ided_abundance <- merge(x =rvm_abundance_xcms_sample_names, y = id_results, by.x = "bin", by.y = "X", all.y = TRUE)
dim(merged_ided_abundance)
#View(merged_ided_abundance)

#convert retention time and abundances from factor to numeric
#lost some precision!
#col
#for(i in 2:48){
#  merged_ided_abundance[,i] <- as.numeric(merged_ided_abundance[,i])
#}
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
    if(length(get_rows)>2){
      ### print the abundances
      id_abundance_subset <- merged_ided_abundance[get_rows,]
      #View(id_abundance_subset)
      #summary(id_abundance_subset)
      brew_colors <- brewer.pal(n = 8, name = "Greys")
      control_plot <- ggplot(id_abundance_subset, aes(x=Sample)) + 
        geom_line(aes(y = CON13, x=Sample),size=1.1, color = brew_colors[3]) +
        geom_line(aes(y = CON14, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = CON15, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = CON16, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = CON17, x=Sample),size=1.1, color = brew_colors[7]) +
        geom_line(aes(y = CON18, x=Sample),size=1.1, color = brew_colors[8]) +
        labs(title = id) +
        ggtitle(paste(id, ": Control")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
                             axis.text.x=element_blank(),
                             axis.ticks.x=element_blank())
      
      brew_colors <- brewer.pal(n = 8, name = "Blues")
      l_plot <- ggplot(id_abundance_subset, aes(x=Sample)) + 
        geom_line(aes(y = L13, x=Sample),size=1.1, color = brew_colors[3]) +
        geom_line(aes(y = L14, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = L15, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = L16, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = L17, x=Sample),size=1.1, color = brew_colors[7]) +
        geom_line(aes(y = L18, x=Sample),size=1.1, color = brew_colors[8]) +
        labs(title = id) +
        ggtitle(paste(id, ": L")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      brew_colors <- brewer.pal(n = 8, name = "Reds")
      n_plot <- ggplot(id_abundance_subset, aes(x=X)) + 
        geom_line(aes(y = N13, x=Sample),size=1.1, color = brew_colors[3]) +
        geom_line(aes(y = N14, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = N15, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = N16, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = N17, x=Sample),size=1.1, color = brew_colors[7]) +
        geom_line(aes(y = N18, x=Sample),size=1.1, color = brew_colors[8]) +
        labs(title = id) +
        ggtitle(paste(id, ": N")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      brew_colors <- brewer.pal(n = 8, name = "Greens")
      z_plot <- ggplot(id_abundance_subset, aes(x=X)) + 
        geom_line(aes(y = Z13, x=Sample),size=1.1, color = brew_colors[3]) +
        geom_line(aes(y = Z14, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = Z15, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = Z16, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = Z17, x=Sample),size=1.1, color = brew_colors[7]) +
        geom_line(aes(y = Z18, x=Sample),size=1.1, color = brew_colors[8]) +
        labs(title = id) +
        ggtitle(paste(id, ": Z")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())      
      
      brew_colors <- brewer.pal(n = 8, name = "Purples")
      ln_plot <- ggplot(id_abundance_subset, aes(x=X), color = "chartreuse2") + 
        geom_line(aes(y = LN13, x=Sample),size=1.1, color = brew_colors[3]) +
        geom_line(aes(y = LN14, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = LN15, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = LN16, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = LN17, x=Sample),size=1.1, color = brew_colors[7]) +
        geom_line(aes(y = LN18, x=Sample),size=1.1, color = brew_colors[8]) +
        labs(title = id) +
        ggtitle(paste(id, ": LN")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      brew_colors <- brewer.pal(n = 8, name = "PuBu")
      zl_plot <- ggplot(id_abundance_subset, aes(x=X)) + 
        geom_line(aes(y = ZL15, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = ZL16, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = ZL17, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = ZL18, x=Sample),size=1.1, color = brew_colors[7]) +
        labs(title = id) +
        ggtitle(paste(id, ": ZL")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      brew_colors <- brewer.pal(n = 8, name = "YlOrBr")
      zn_plot <- ggplot(id_abundance_subset, aes(x=X), color = "darkgoldenrod2") + 
        geom_line(aes(y = ZN13, x=Sample),size=1.1, color = brew_colors[3]) +
        geom_line(aes(y = ZN14, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = ZN15, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = ZN16, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = ZN17, x=Sample),size=1.1, color = brew_colors[7]) +
        geom_line(aes(y = ZN18, x=Sample),size=1.1, color = brew_colors[8]) +
        labs(title = id) +
        ggtitle(paste(id, ": ZN")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
      
      brew_colors <- brewer.pal(n = 8, name = "BuPu")
      zln_plot <- ggplot(id_abundance_subset, aes(x=X), color = "darkmagenta") + 
        geom_line(aes(y = ZLN13, x=Sample),size=1.1, color = brew_colors[3]) +
        geom_line(aes(y = ZLN14, x=Sample),size=1.1, color = brew_colors[4]) +
        geom_line(aes(y = ZLN15, x=Sample),size=1.1, color = brew_colors[5]) +
        geom_line(aes(y = ZLN16, x=Sample),size=1.1, color = brew_colors[6]) +
        geom_line(aes(y = ZLN17, x=Sample),size=1.1, color = brew_colors[7]) +
        geom_line(aes(y = ZLN18, x=Sample),size=1.1, color = brew_colors[8]) +
        labs(title = id) +
        ggtitle(paste(id, ": ZLN")) +
        xlab("retention time") +
        ylab("abundance") +
        theme_classic()
      
      ###plot the mean log abundances
      id_results_subset <- id_results[get_rows,]
      id_results_subset$retention_time <- id_abundance_subset$Sample
      #View(id_results_subset)
      #plotting X instead of retention_time because not enough precision in retention_time
      rt_plot <- ggplot(id_results_subset, aes(x=X)) + 
        #ggtitle("title") +
        geom_smooth(method='loess', formula = y ~ x, aes(y = c_mean, x=retention_time, color = "c_mean"), size=1.5, se=F) + 
        geom_smooth(method='loess', formula = y ~ x, aes(y = n_mean, x=retention_time, color="n_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = l_mean, x=retention_time, color="l_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = z_mean, x=retention_time, color="z_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = ln_mean, x=retention_time, color="ln_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = zn_mean, x=retention_time, color="zn_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = zl_mean, x=retention_time, color="zl_mean"), se=F) +
        geom_smooth(method='loess', formula = y ~ x, aes(y = zln_mean, x=retention_time, color="zln_mean"), se=F) +
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
        xlab("retention time)") +
        ylab("log(abundance)") +
        #ggtitle(id) #+
        theme_classic() +
        theme(legend.position = "bottom")
      
      #plot(rt_plot)
      #ggsave(filename = paste(rvm_graphics, "/rt_", id, "_image.png", sep=""))
      
      colnames(id_results_subset)
      colnames(id_abundance_subset)
      dim(id_results_subset)
      dim(id_abundance_subset)
      id_results_subset$retention_time <- id_abundance_subset$Sample
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

