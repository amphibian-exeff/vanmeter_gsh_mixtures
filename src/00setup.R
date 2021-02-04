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
View(rvm_cort)
