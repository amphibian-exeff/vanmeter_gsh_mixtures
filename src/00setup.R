#Install and load supporting libraries.
print(Sys.info()[4])

R.Version()$version.string
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(MASS)
library(FSA)
library(car)
library(rcompanion)
library(moments)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(RColorBrewer)
library(gt)
library(gridExtra)
library(patchwork)
library(webshot)
#webshot::install_phantomjs()


print("list of loaded packages: ")
print((.packages()))

#tom epa windows
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  rvm_root <- file.path("c:", "git", "VanMeteretal2021_fp_mixtures")
}
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  rvm_root <- file.path("c:","git","VanMeteretal2021_fp_mixtures")
}

print(paste("Root directory location: ", rvm_root, sep=""))

rvm_csv_in <- file.path(rvm_root, "data_in")
rvm_data_out <- file.path(rvm_root, "data_out")
rvm_graphics <- file.path(rvm_root, "graphics")

#check to see if directories are accessible
boo = file.exists(file.path(rvm_csv_in,"/gsf_data.csv"))
print(paste("check to see if R can access GSF file OK: ", boo))

boo = file.exists(file.path(rvm_csv_in,"/atrazine_data.csv"))
print(paste("check to see if R can access atrazine file OK: ", boo))

boo = file.exists(file.path(rvm_csv_in,"/alachlor_data.csv"))
print(paste("check to see if R can access alachlor file OK: ", boo))

#cleaned up data set, manually reshaped, long format
rvm_cort <- read.csv(file.path(rvm_csv_in,"/gsf_data.csv"), stringsAsFactors = TRUE)
rvm_atrazine <- read.csv(file.path(rvm_csv_in,"/atrazine_data.csv"), stringsAsFactors = TRUE)
rvm_alachlor <- read.csv(file.path(rvm_csv_in,"/alachlor_data.csv"), stringsAsFactors = TRUE)
rvm_cort_drop_interactions <- read.csv(file.path(rvm_csv_in,"/gsf_data_drop_interactions.csv"), stringsAsFactors = TRUE)

# get the retention time class names--metabonalyst (dropped)
temp_rvm_abundance_metaboanalyst <- read.csv(file.path(rvm_csv_in,"/rjvm_livers_19085_metaboanalyst.csv"), stringsAsFactors = TRUE)
dim(temp_rvm_abundance_metaboanalyst)
colnames(temp_rvm_abundance_metaboanalyst)
#retention_time <- as.character(temp_rvm_abundance_metaboanalyst$Sample)[2:2794]

# get the retention time class names--xcms
temp_rvm_abundance_xcms <- read.csv(file.path(rvm_csv_in,"/rjvm_livers_19085_xcms.csv"), stringsAsFactors = TRUE)
dim(temp_rvm_abundance_xcms)
colnames(temp_rvm_abundance_xcms)
retention_time <- as.character(temp_rvm_abundance_xcms$Sample)[2:2957]

#abundance data
rvm_abundance_metabonanalyst <- read.csv(file.path(rvm_csv_in,"/rjvm_livers_metaboanalyst_transposed.csv"), stringsAsFactors = TRUE)
rvm_abundance_metabonanalyst$Class

#abundance data
rvm_abundance_xcms <- read.csv(file.path(rvm_csv_in,"/rjvm_livers_xcms_transposed.csv"), stringsAsFactors = TRUE)
rvm_abundance_xcms$Class


summary(rvm_cort)
colnames(rvm_cort)
# [1] "ID"        "Z"         "L"         "N"         "treatment" "GSH_nM_mL"
class(rvm_cort$ID)
levels(rvm_cort$ID)
class(rvm_cort$Z)
class(rvm_cort$L)
class(rvm_cort$N)
class(rvm_cort$treatment)
levels(rvm_cort$treatment)

dim(rvm_cort)
#View(rvm_cort)
