### NC habitat file generation for BeeHAVE

###04 pollen uptake

#Edited by E. Paulukonis Jan 2022


df<-patch_list_output[[2]]  #single apiary, contains all the data by date and such for each 


look<-df %>% filter(CDLName == 'LC_1')






testz<-patch_list_output[[1]]
testz$patchType<-paste(testz$LC_Class,testz$patch, sep="")





write.csv(testz, paste0(root_data_out,"/example_dataset.csv"))







# for date x...
# for each unique value (patch) in testy
# if colname(testy)== CDLName pull out rows in formatted csv that match class
# then; sample the available plants from formatted pollen using proportions as probabilities





testx<-rasterFromXYZ(testy) 
testx<-setExtent(testx,extent(test))
plot(testx)


data_frame_mod <- data_frame[which(data_frame$col1 %in% c("b","e")
                                   | data_frame$col2 > 4),]

lc_data[lc_data$DateCode == 1 & lc_data$CDLClass == class,]

ifelse(df_patch_by_date$class == df_lc_by_date$CDLClass, )


# Let's test this with a single apiary, and a single LC class first. 
ap_pts_a<-ap_pts[ap_pts$Apiary == 'A',]
plot(patch_by_lc[[1]][[1]])
plot(ap_pts_a, add=T)

testr<-patch_by_lc[[1]][[1]] #test with single LC class (1) 
# plot(testr) #visual check

testp <- disaggregate(rasterToPolygons(testr,dissolve=T))# convert to vector to get distance, use dissolve to keep raster values combined and disaggregate to keep individual features
# plot(testp) # visual check 
distance_p<-gDistance(ap_pts_a, testp, byid=TRUE) # this is a set of distances from apiary a to each patch of LC 1



