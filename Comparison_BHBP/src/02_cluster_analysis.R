### NC habitat file generation for BeeHAVE

###02 Cluster Analysis

#Edited by E. Paulukonis Dec 2021

import_start_time <- Sys.time()
print("stepping into 02_cluster_analysis.R")

#### Pull out Apiary D
apiary_d<-ap_pts[4,]
buf_r <- gBuffer(apiary_d, width=1609)  # 1 mile buffer


#1 all landcover classes get same pollen/nectar assignment
crop_out<-crop(lc_data_sf, buf_r)
mask_out<-raster::mask(crop_out, buf_r)
plot(mask_out) #shows raw cdl in buffer

#reclass 
m <- c(1, 36, 1, 38, 100, 1, 200, 254, 1,36, 37, 176, 110,111,NA, 123,124,NA) #change all crops to 1, change hay to grassland, remove water and high intensity urban
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(mask_out, rclmat)

#take a peek
plot(rc)
unique(values(rc))


#get clump info  
  clVal <- unique(rc)
  list_clump<-list() #list for each apiary
  for (i in clVal) {
    list_clump_names<-list()
    # create & fill in class raster
    r.class <- setValues(raster(rc), NA)
    r.class[rc == i]<- 1
    # clump class raster, using 4 neighbor rule (more discrete patches)
    clp <- clump(r.class, directions=4)
    
    is<-unique(values(clp))
    becomes<-c(is-1)
    rcm<-cbind(is,becomes)
    clp<- reclassify(clp, rcm)

    # put each patch landscape into the list
    list_clump[[i]]<-clp
    names(list_clump[[i]])<-paste0("LC_",as.character(i))
    list_clump[lengths(list_clump) == 0] <- NULL
  }

plot(list_clump[[1]])


