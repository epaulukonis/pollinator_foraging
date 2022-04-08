### NC habitat file generation for BeeHAVE

###03 Sample Patches and assign pollen 

#Edited by E. Paulukonis Dec 2021

import_start_time <- Sys.time()
print("stepping into 03_sample_patches.R")

lc_data<-read.csv(file = file.path(root_data_in, "LC/LC_data.csv"))
pollen_data<-read.csv(file = file.path(root_data_in, "pollen_data.csv"))
lc_cw<-read.csv(file = file.path(root_data_in, "LC/LC_crosswalk.csv"))
date_cw<-read.csv(file = file.path(root_data_in, "Dates/Date_crosswalk.csv"))
date_y<-read.csv(file = file.path(root_data_in, "Dates/year_days.csv"))
forage<-read.csv(file = file.path(root_data_in, "foraging_data_by_plant.csv"))



##### Sample species by pollen proportion for each individual apiary's set of LC classes by date ----
# triple nested for loop to output nested list, runs quickly
patch_list_output<-list()
for (a in 1:length(patch_by_lc)){ #number of apiaries
  a=1
  lc_list<-list() # create empty list to hold output
  apiary<-patch_by_lc[[a]] # pull apiary out
  
  #get number of landcover classes in the apiary radius
  for (lc in 1:length(apiary)){
    lc=1
    #get area and distance of each patch for the lc class by apiary first
    dar<-apiary[[lc]] #individual LC class
    
    #patch area 
    distancearea_a<-as.data.frame(tapply(area(dar), dar[], sum))
    names(distancearea_a)<-'area'
    distancearea_a$patch<-row.names(distancearea_a)
    
    #distance (units based on projection)
    ap_pts_a<-ap_pts[ap_pts$ID == a,]
    distp <- disaggregate(rasterToPolygons(dar,dissolve=T))# convert to vector to get distance, use dissolve to keep raster values combined and disaggregate to keep individual features
    distance_p<-gDistance(ap_pts_a, distp, byid=TRUE)
    distancearea_a$distance_m<-distance_p
    distancearea_a$distance_m<-ifelse(distancearea_a$distance_m == 0, 0.000001, distancearea_a$distance_m)
    colnames(distancearea_a)[3]<-"distance_m"
    
    #detection probability (decay with distance function, Becher et al. 2016)
    distancearea_a$calculatedDetectionProb_per_trip<-exp(1)^(-0.00073*distancearea_a$distance_m)
    distancearea_a$modelledDetectionProb_per_trip <-0
    
    #foraging time
    distancearea_a$pollenGathering_s<- 600
    distancearea_a$nectarGathering_s<- 1200
    
    #empty pesticide per nectar
    distancearea_a$nectarPesticide_ng_per_g<- 0  #this should technically be added below but I forgot to make it pretty
    
    # create matrix for individual patches; this code creates and organizes the matrix
    lc_class<-as.data.frame(apiary[[lc]], xy=T, na.rm=T) # get individual lc class
    patches<-unique(lc_class[,3]) #get unique patch numbers
    dates<-1:365 #get dates
    patch_df<-as.data.frame(matrix(data=0,nrow=length(patches)*length(dates),ncol=4)) # create matrix for patch and date data
    names(patch_df)<-c('Patch','Day','Class','Plant')
    dates_data<-rep(1:365,each=length(patches))
    patch_data<-rep(1:length(patches),times=365)
    patch_df[,1]<-patch_data
    patch_df[,2]<-dates_data
    patch_df[,3]<-colnames(lc_class)[3]
    
    patch_df_day<-patch_df %>% group_by(Patch) %>% left_join(select(date_y, Day, DateCode), by = "Day")
    
    #for each patch, this samples a row number from the pollen data frame corresponding to a plant
    #essentially, for each date, we sample the n patches and put that in a data frame, which we add to the output list
    patch_list<-list()
      for(date in 1:length(dates)){ #run through the dates; we will sort them by patch once we unlist the output later
      # get class value
      class<-colnames(lc_class)[3] # get class value
      # pull out individual date
      df_patch_by_date<-patch_df_day[patch_df_day$Day== date,] 
      
      #grab specific DateCode, which represents the period that can be sampled from
      DateCode <-unique(df_patch_by_date$DateCode)
      df_lc_by_date<-lc_data[which(lc_data$CDLName %in% class & lc_data$DateCode == DateCode ),] 
      
      # pulls out plant probabilities based on pollen count
      df_pollen_by_date_and_lc<-pollen_data[which(pollen_data$DateCode == DateCode & pollen_data$Common %in% df_lc_by_date$Common),]
      
      #order the plants by the descending pollen probabilities
      df_lc_by_date<-df_lc_by_date[order(match(df_lc_by_date$Common, df_pollen_by_date_and_lc$Common.Name)), ]
    
      # samples from the available species for the LC and date, while accounting for dates where no pollen is found
      if(df_pollen_by_date_and_lc$DateCode == 0 & df_pollen_by_date_and_lc$Pollen == 0){
        r1<-as.data.frame(matrix(0, nrow=length(unique(patch_df_day$Patch)), ncol=1))
        df_lc_by_date$ID<-0  
      }else{
        r1<-data.frame(sample(1:nrow(df_pollen_by_date_and_lc), size = length(patches), replace = TRUE, prob = df_pollen_by_date_and_lc$Pollen))
        df_lc_by_date$ID<-1:nrow(df_lc_by_date)
      }

      #this outputs a set of sampled  (or not sampled, just 0) patch points by date
      #ID here is what we use to merge the patch points 
      colnames(r1)[1]<-'ID'
      r1$patch<-as.numeric(row.names(r1))-1
      r1$Day<-date
      # merge the sampled dataset with the LC dataset containing the names of the plant species and patch IDs
      lc_sampled_pollen<-merge(r1,df_lc_by_date,by='ID', by.all=T) 
      lc_sampled_pollen<-merge(lc_sampled_pollen, distancearea_a, by='patch', by.all=T) # add area and distance of each patch
      lc_sampled_pollen<-dplyr::left_join(lc_sampled_pollen, select(forage, Plant, Pollen_g_m2, Nectar_ml_per_m2), by = c("Common" = "Plant"))
      # turn NA pollen data to 0
      lc_sampled_pollen[is.na(lc_sampled_pollen)] <- 0
      lc_sampled_pollen<-lc_sampled_pollen[order(lc_sampled_pollen$Day, as.numeric(lc_sampled_pollen$patch)),]
      # put into the empty list for each date
      patch_list[[date]]<-lc_sampled_pollen 
    }
    #put into the empty list for each land cover class
    patch_list<-do.call(rbind.data.frame, patch_list) # unlist each individual date into a single lc class dataframe
    patch_list$apiary<-a # add the apiary
    patch_list$patchType<-paste(patch_list$LC_Class, patch_list$patch, sep="") #add the unique identifier
    
    #patch_list<-patch_list[order(patch_list$patch),] #order by patch 
    
    #format it like the schmolke file; this section is dependent on how you're running it
    patch_list$xcor<-0
    patch_list$ycor<-0
    patch_list$concentration <- 0
    patch_list$pollenPesticide_ng_per_g<-0
    #patch_list_sch<-patch_list[,c(3,2,1,19,11,20,21,10,16,22,17,12,13,15,14,23)]
patch_list_t<-patch_list[,c(1:3,10:18,20:24)]  #remove the columns we don't need


#order it in line with the schmolke file; note that the schmolke file does not have a nectar pesticide concentration  
patch_list_f<-patch_list_t[,c(3,2,1,13,5,14:15,4,11,16,12,6,7,9,8,17,10)]
patch_list_f$ID<-patch_list_f$patch

patch_list_f <- patch_list_f[order(patch_list_f$patch),]
  
    lc_list[[lc]]<-patch_list_f #this contains the list of each LC patch by date for each apiary (12 LCs each with 7 nested lists)
    lc_list_out<-do.call(rbind.data.frame,lc_list) #unlist the apiary specific data into a single dataframe
    

    #write.csv(lc_list_out, paste0(root_data_out,"/habitat_files/apiary_",a,"_habitat_file_5m.csv")) 
    
    write.table(lc_list_out,paste0(root_data_out,"/habitat_files/apiary_",a,"_habitat_file_test.txt"), row.names = FALSE)
    
    patch_list_output[[a]]<-lc_list_out #put the df for each apiary into a list
  }
}

#sort by day and THEN by patch




##### Sample species by pollen proportion for each individual apiary's set of LC classes by individual sampling date ----
# triple nested for loop to output nested list, runs quickly
patch_list_output<-list()
for (a in 1:length(patch_by_lc)){ #number of apiaries
 
 lc_list<-list() # create empty list to hold output
 apiary<-patch_by_lc[[a]] # pull apiary out
 
  for (lc in 1:length(apiary)){
  #get area and distance of each patch for the lc class by apiary first
  dar<-apiary[[lc]] #individual LC class
  
  #area 
  distancearea_a<-as.data.frame(tapply(area(dar), dar[], sum))
  names(distancearea_a)<-'area'
  distancearea_a$patch<-row.names(distancearea_a)
  
  #distance (units based on projection)
  ap_pts_a<-ap_pts[ap_pts$ID == a,]
  distp <- disaggregate(rasterToPolygons(dar,dissolve=T))# convert to vector to get distance, use dissolve to keep raster values combined and disaggregate to keep individual features
  distance_p<-gDistance(ap_pts_a, distp, byid=TRUE)
  distancearea_a$distance<-distance_p
  
  #foraging time
  distancearea_a$pollenGathering_s<- 600
  distancearea_a$nectarGathering_s<- 1200

  # create matrix for individual patches; this code creates and organizes the matrix
  lc_class<-as.data.frame(apiary[[lc]], xy=T, na.rm=T) # get individual lc class
  patches<-unique(lc_class[,3]) #get unique patch numbers
  dates<-1:nrow(date_cw) #get dates
  patch_df<-as.data.frame(matrix(data=0,nrow=length(patches)*length(dates),ncol=4)) # create matrix for patch and date data
  names(patch_df)<-c('patch','date','class','plant')
  dates_data<-rep(1:7,each=length(patches))
  patch_data<-rep(1:length(patches),times=7)
  patch_df[,1]<-patch_data
  patch_df[,2]<-dates_data
  patch_df[,3]<-colnames(lc_class)[3]
  
  #for each patch, this samples a row number from the pollen data frame corresponding to a plant
    patch_list<-list()
    for (date in 1:length(dates)){
      # get class value
      class<-colnames(lc_class)[3] # get class value
      # pull out date
      df_patch_by_date<-patch_df[patch_df$date == date,] 
      # this pulls the specific pollen types corresponding to the date
      df_lc_by_date<-lc_data[which(lc_data$CDLName %in% class & lc_data$DateCode == date ),] 
      # specific IDs corresponding to the plant species
      df_lc_by_date$ID<-1:nrow(df_lc_by_date) 
      # pulls out plant probabilities based on pollen count
      df_pollen_by_date_and_lc<-pollen_data[which(pollen_data$DateCode == date & pollen_data$Common %in% df_lc_by_date$Common),]
      # samples from the available species for the LC and date
      r1<-as.data.frame(sample(nrow(df_pollen_by_date_and_lc), size = length(patches), replace = TRUE, prob = df_pollen_by_date_and_lc$Pollen))
      # add ID (ID here corresponds to the pollen value we sampled from, as we can't use strings)
      colnames(r1)[1]<-'ID'
      r1$patch<-row.names(r1)
      # merge the sampled dataset with the LC dataset containing the names of the plant species and patch IDs
      lc_sampled_pollen<-merge(r1,df_lc_by_date,by='ID', by.all=T) 
      lc_sampled_pollen<-merge(lc_sampled_pollen, distancearea_a, by='patch', by.all=T) # add area and distance of each patch
      # put into the empty list for each date
      patch_list[[date]]<-lc_sampled_pollen
    }
  #put into the empty list for each land cover class
  patch_list<-do.call(rbind.data.frame, patch_list) # unlist each individual date into a single lc class dataframe
  patch_list$apiary<-a # add the apiary
  
  lc_list[[lc]]<-patch_list #this contains the list of each LC patch by date for each apiary (12 LCs each with 7 nested lists)
  lc_list_out<-do.call(rbind.data.frame,lc_list) #unlist the apiary specific data into a single dataframe
  patch_list_output[[a]]<-lc_list_out #put the df for each apiary into a list
  }
}

#the output for this should be a list of individual data-frames containing all the unique LC patches with their sampled vegetation by date for each apiary

patch_list_output[[1]]
