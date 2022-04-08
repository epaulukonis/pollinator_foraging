### NC habitat file generation for BeeHAVE

###02 Cluster Analysis

#Edited by E. Paulukonis Dec 2021

import_start_time <- Sys.time()
print("stepping into 02_cluster_analysis.R")

#### Divide into list of 10 apiaries ----

# In this section, we split the apiary data into a list and  apply a cluster analysis to determine a patch landscape for each cover class

# split into list of all apiaries, and buffer each by the set distance
ap_pts_s<-split(ap_pts, ap_pts$Apiary, drop = FALSE)
buff<-function(x){
  buf_r <- gBuffer(x,width=1609)  # 1 mile
}
ap_pts_b<-lapply(ap_pts_s, buff)


# for each apiary, crop and mask the LC data, and reclassify the values to combine hay/pasture/grass and cultivated crops, remove 111 (open water)
create_buffered_lc<-function(x){
  crop_out<-crop(lc_data_sf, x)
  mask_out<-raster::mask(crop_out, x)
  m <- c( 0, 36, 1, 38, 100, 1, 200, 254, 1,36, 37, 176, 110,111,NA)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  rc <- reclassify(mask_out, rclmat)
}

lc_buffered<-lapply(ap_pts_b,create_buffered_lc) #this is a list of individual buffered regions around each apiary



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

