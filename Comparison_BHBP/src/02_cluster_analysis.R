### NC habitat file generation for BeeHAVE

###02 Cluster Analysis

#Edited by E. Paulukonis Dec 2021

import_start_time <- Sys.time()
print("stepping into 02_cluster_analysis.R")

#### Pull out Apiary D


apiary_d<-ap_pts[4,]
buf_r <- gBuffer(apiary_d, width=1609)  # 1 mile buffer

crop_out<-crop(lc_data_sf, buf_r)
mask_out<-raster::mask(crop_out, buf_r)
m <- c( 0, 36, 1, 38, 100, 1, 200, 254, 1,36, 37, 176, 110,111,NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(mask_out, rclmat)

plot(rc)



# create a nested for loop that conducts a 4 neighbor (rook) cluster analysis on each land cover type contained within each apiary and outputs it to a nested list
patch_by_lc<-list()
for (l in 1:length(lc_buffered)){
  r<-lc_buffered[[l]]
  clVal <- unique(r)
  list_clump<-list() #list for each apiary
  for (i in clVal) {
    list_clump_names<-list()
    # create & fill in class raster
    r.class <- setValues(raster(r), NA)
    r.class[r == i]<- 1
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
  # add each buffered apiary to the final nested list
  patch_by_lc[[l]]<-list_clump
}


#check that it plots correctly
plot(lc_buffered$A) #apiary; all landscapes
plot(patch_by_lc[[2]][[1]]) #apiary, patch

