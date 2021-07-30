library(RColorBrewer)
library(gplots)
library(pheatmap)

#spectra data
rvm_heatmap_peaks_all <- read.csv(file.path(rvm_csv_in,"/rvm_summary_stats_table_xcms_peak_ID_main.csv"), stringsAsFactors = TRUE)
dim(rvm_heatmap_peaks_all)
colnames(rvm_heatmap_peaks_all)
rownames(rvm_heatmap_peaks_all)

rvm_heatmap_peaks_cols <- rvm_heatmap_peaks_all[,c(8:14)]
rownames(rvm_heatmap_peaks_cols) <- rvm_heatmap_peaks_all$peak_id

rvm_heatmap_peaks <- rvm_heatmap_peaks_cols[which(rowSums(rvm_heatmap_peaks_cols[1:3], na.rm=T)>0),]


View(rvm_heatmap_peaks)
colnames(rvm_heatmap_peaks)

rvm_heatmap_peaks$z_fvalue[which(is.na(rvm_heatmap_peaks$z_fvalue))] <- 0.9
rvm_heatmap_peaks$l_fvalue[which(is.na(rvm_heatmap_peaks$l_fvalue))] <- 0.9
rvm_heatmap_peaks$n_fvalue[which(is.na(rvm_heatmap_peaks$n_fvalue))] <- 0.9
rvm_heatmap_peaks$zl_fvalue[which(is.na(rvm_heatmap_peaks$zl_fvalue))] <- 0.9
rvm_heatmap_peaks$zn_fvalue[which(is.na(rvm_heatmap_peaks$zn_fvalue))] <- 0.9
rvm_heatmap_peaks$ln_fvalue[which(is.na(rvm_heatmap_peaks$ln_fvalue))] <- 0.9
rvm_heatmap_peaks$zln_fvalue[which(is.na(rvm_heatmap_peaks$zln_fvalue))] <- 0.9


colnames(rvm_heatmap_peaks) <- c("Atrazine (Z)", "Alachlor (L)", "Urea (N)",
                                 "Z:L", "Z:N", "L:N", "Z:L:N")

View(rvm_heatmap_peaks)
pheatmap(as.matrix(-log(rvm_heatmap_peaks)), dendrogram="none", col = coul)

# display.brewer.all()
coul <- colorRampPalette(brewer.pal(8, "YlOrRd"))(18)
heatmap_png <- pheatmap(as.matrix(-log(rvm_heatmap_peaks)), dendrogram="none", col = coul)
png(file.path(rvm_graphics, "heatmap_mixtures_main_effects.png"),
    width=5, height=8, units="in", res=300)
  print(heatmap_png)
dev.off()
