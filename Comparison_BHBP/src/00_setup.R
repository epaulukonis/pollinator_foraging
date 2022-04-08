### NC habitat file generation for BeeHAVE

###00 Setup of directories and file paths

#Edited by E. Paulukonis Nov 2021


#Install and load supporting libraries.
print("stepping into 00_setup.R")
print(Sys.info()[4])
R.Version()$version.string
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(maptools)
library(igraph)
library(stars)


who_is_running<-'eap'
#who_is_running<-'stp'
if(Sys.info()[4]=="LZ26EPAULUKO"){
  root_dir <- 'C:/Users/epauluko/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/Paulukonis_Documents/manuscript_BEEHAVE/HabitatFile_NC'
}else{
  root_dir <- file.path("/work", "HONEYBEE", who_is_running, "BeeHave_BBPN")
}
print(root_dir)

root_data_in <- file.path(root_dir, "data_in")
print(root_data_in)
root_data_out <- file.path(root_dir, "data_out")
print(root_data_out)
root_figures <- file.path(root_dir, "figures")
print(root_figures)
root_src <- file.path(root_dir, "src")
print(root_src)


#source other files
#source(file.path(root_src, "01_study_area.R"))
#source(file.path(root_src, "02_cluster_analysis.R"))
#source(file.path(root_src, "03_sample_patches.R"))
#source(file.path(root_src, "04_pollen_uptake.R"))
