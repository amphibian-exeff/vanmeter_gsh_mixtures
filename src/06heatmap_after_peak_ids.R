rvm_peak_ids_file <- paste(rvm_csv_in,"/initial_peak_ids.csv",sep="")
id_results <- read.csv(rvm_peak_ids_file)
dim(id_results)
colnames(id_results)

id_list <- unique(id_results$ID)
id_list

#plot retention_time v 
#c_mean	n_mean	l_mean	ln_mean	z_mean	zn_mean	zl_mean	zln_mean

rt_pdf_filename <- paste(rvm_graphics,"/rvm_rt_v_abundance_significant_ids.pdf",sep="")
pdf(rt_pdf_filename, width = 8.5, height = 11, onefile = TRUE)
par(mfrow=c(1,1))
  for(id in id_list){
    print(id)
    get_rows <- which(id_results$ID==id)
    id_results_subset <- id_results[get_rows,]
    rt_plot <- ggplot(id_results_subset, aes(x=retention_time)) + 
      #ggtitle("title") +
      geom_line(aes(y = c_mean, x=retention_time, color = "c_mean"),size=1.5) + 
      geom_line(aes(y = n_mean, x=retention_time, color="n_mean")) +
      geom_line(aes(y = l_mean, x=retention_time, color="l_mean")) +
      geom_line(aes(y = z_mean, x=retention_time, color="z_mean")) +
      geom_line(aes(y = ln_mean, x=retention_time, color="ln_mean")) +
      geom_line(aes(y = zn_mean, x=retention_time, color="zn_mean")) +
      geom_line(aes(y = zl_mean, x=retention_time, color="zl_mean")) +
      geom_line(aes(y = zln_mean, x=retention_time, color="zln_mean")) +
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
      xlab("retention time") +
      ylab("abundance") +
      #ggtitle(id) #+
      theme_classic()
    
    #ggsave(filename = paste(rvm_graphics, "/rt_", id, "_image.png", sep=""))
    
    rt_table <- id_results_subset %>%
      dplyr::select(-ID, -c_mean, -n_mean, -l_mean, -ln_mean, -z_mean,
                    -zn_mean, -zl_mean, -zln_mean, -z_fvalue, -l_fvalue, -n_fvalue,
                    -zl_fvalue, -zn_fvalue, -ln_fvalue, -zln_fvalue,
                    -significant05., -significant01., -main_significant05., -main_significant01.) %>%
      gt() %>%
      tab_header(
        title = id
      ) %>%
      fmt_number(
        columns = vars(z_fvalue.1, l_fvalue.1, n_fvalue.1,
                       zl_fvalue.1, zn_fvalue.1, ln_fvalue.1,
                       zln_fvalue.1)
      )
    
    gtsave(rt_table, filename = paste(rvm_graphics, "/rt_", id, "_table.png", sep=""))
    #boxc <- list(rt_plot, as_grob(rt_table))
    #ggarrange(plotlist=boxc, nrow = 2)  
    #grid.arrange(rt_plot, rt_table, nrow = 2)
    
    #tmp <- tempfile(fileext = '.png') #generate path to temp .png file
    #gtsave(rt_table, tmp) #save gt table as png
    png_table_filename = paste(rvm_graphics, "/rt_", id, "_table.png", sep="")
    table_png <- png::readPNG(png_table_filename, native = TRUE) # read tmp png file
    
    rt_combined <- rt_plot / table_png
    
    plot(rt_combined)
    #ggsave(filename = paste(rvm_graphics, "/rt_", id, "_combined.png", sep=""))
  }
dev.off()

