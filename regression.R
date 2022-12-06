library(tidyverse)
library(dplyr)
library(leaps)
library(car)
setwd("/Users/../sh_metro")
getwd()
# dataframe for regression analysis
# 横轴时间， 纵轴出口编号
shanxi_out = read.csv('shanxi_out.csv')
shanxi_in = read.csv('shanxi_in.csv')
renmin_out = read.csv('renmin_out.csv')
renmin_in = read.csv('renmin_in.csv')
jiangsu_out = read.csv('jiangsu_out.csv')
jiangsu_in = read.csv('jiangsu_in.csv')

shanxi_in$all = rowSums(shanxi_in[,c(2:20)])
shanxi_in$mor = rowSums(shanxi_in[,c(4:6)])
shanxi_in$aft = rowSums(shanxi_in[,c(14:16)])

jiangsu_in$all = rowSums(jiangsu_in[,c(2:20)])
jiangsu_in$mor = rowSums(jiangsu_in[,c(4:6)])
jiangsu_in$aft = rowSums(jiangsu_in[,c(14:16)])

renmin_in$all = rowSums(renmin_in[,c(2:20)])
renmin_in$mor = rowSums(renmin_in[,c(4:6)])
renmin_in$aft = rowSums(renmin_in[,c(14:16)])

shanxi_out$all = rowSums(shanxi_out[,c(2:20)])
shanxi_out$mor = rowSums(shanxi_out[,c(4:6)])
shanxi_out$aft = rowSums(shanxi_out[,c(14:16)])

jiangsu_out$all = rowSums(jiangsu_out[,c(2:20)])
jiangsu_out$mor = rowSums(jiangsu_out[,c(4:6)])
jiangsu_out$aft = rowSums(jiangsu_out[,c(14:16)])
jiangsu_out = rbind(jiangsu_out,c(0,0,0,0,0,0,0,))
renmin_out$all = rowSums(renmin_out[,c(2:20)])
renmin_out$mor = rowSums(renmin_out[,c(4:6)])
renmin_out$aft = rowSums(renmin_out[,c(14:16)])

shanxi_area = st_read("shanxi_service_area.shp")
jiangsu_area = st_read("jiangsu_service_area.shp")
renmin_area = st_read("renmin_service_area.shp")

poi_residential = st_read('poi_residential.shp')
poi_commercial = st_read('poi_commercial.shp')
poi_public = st_read('poi_public.shp')
poi_office = st_read('poi_office.shp')

building = st_read('sh_building.shp')
#plot(poi_office$geometry)


buf_dist = 3000

shanxi_s_df = count_feature_in_sector(shanxi_s_sector,shanxi_exit, buf_dist,shanxi_area)
shanxi_s_df$ref = as.numeric(as.character(shanxi_exit$ref))

#shanxi_e_df = count_feature_in_sector(shanxi_e_sector,shanxi_exit, buf_dist,shanxi_area)
#shanxi_e_df$ref = as.numeric(as.character(shanxi_exit$ref))


jiangsu_s_df = count_feature_in_sector(jiangsu_s_sector,jiangsu_exit, buf_dist,jiangsu_area)
jiangsu_s_df$ref = as.numeric(as.character(jiangsu_exit$ref))

#jiangsu_e_df = count_feature_in_sector(jiangsu_e_sector,jiangsu_exit, buf_dist,jiangsu_area)
#jiangsu_e_df$ref = as.numeric(as.character(jiangsu_exit$ref))

renmin_s_df = count_feature_in_sector(renmin_s_sector,renmin_exit, buf_dist,renmin_area)
renmin_s_df$ref = as.numeric(as.character(renmin_exit$ref))

#renmin_e_df = count_feature_in_sector(renmin_e_sector,renmin_exit, buf_dist,renmin_area)
#renmin_e_df$ref = as.numeric(as.character(renmin_exit$ref))



shanxi_s_df = left_join(shanxi_s_df, shanxi_out[,c(1,21:24)], by= 'ref')
#shanxi_e_df = left_join(shanxi_e_df, shanxi_in[,c(1,21:24)], by= 'ref')
jiangsu_s_df = left_join(jiangsu_s_df, jiangsu_out[,c(1,21:24)], by= 'ref')
#jiangsu_e_df = left_join(jiangsu_e_df, jiangsu_in[,c(1,21:24)], by= 'ref')
jiangsu_s_df[8,c(11:13)] = c(3140,0,0)
jiangsu_s_df$X= as.character(jiangsu_s_df$X)
jiangsu_s_df[8,10]="江苏路5"

#jiangsu_e_df[8,c(11:13)] = c(3140,0,0)
#jiangsu_e_df$X= as.character(jiangsu_e_df$X)
#jiangsu_e_df[8,10]="江苏路5"
# for angle = 150,135, remove 6, 8,10, 16
renmin_s_df = left_join(renmin_s_df, renmin_out[,c(1,21:24)], by= 'ref')
renmin_s_df  = renmin_s_df[-c(6,8,10,16),]

#renmin_e_df = left_join(renmin_e_df, renmin_in[,c(1,21:24)], by= 'ref')
#renmin_e_df  = renmin_e_df[-c(6,8,10),]
pop_out = rbind(shanxi_s_df,jiangsu_s_df,renmin_s_df)
pop_out = na.omit(pop_out)
pop_out$all = as.numeric(pop_out$all)
#pop_in = rbind(shanxi_e_df,jiangsu_e_df,renmin_e_df)
#pop_in = na.omit(pop_in)
write.csv(pop_out_135,'pop_out_135.csv')

#normalize = function(x){
  return((x - min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x,na.rm = TRUE)))
}
getwd()
#normalize = function(x){
  return((x - mean(x,na.rm = TRUE))/sd(x,na.rm = TRUE))
}
#norm_in =  as.data.frame(cbind(apply(pop_in[,c(2,3,4,5,6,7,8)],2,normalize),pop_in$all))
#norm_out =  as.data.frame(cbind(apply(pop_out[,c(2,3,4,5,6,7,8)],2,normalize),pop_out$all))

#norm_in =  as.data.frame(apply(pop_in[,c(2,3,4,5,6,7,8,11)],2,normalize))
#norm_out =  as.data.frame(apply(pop_out[,c(2,3,4,5,6,7,8,11)],2,normalize))

#pca_in <- prcomp(t(pop_in[,c(2,3,4,5,6,7,8)]), scale=TRUE) 
#norm_in = as.data.frame(cbind(pca_in[["rotation"]],pop_in$all))
#pca_out <- prcomp(t(pop_out[,c(2,3,4,5,6,7,8)]), scale=TRUE) 
#norm_out = as.data.frame(cbind(pca_out[["rotation"]],pop_out$all))
#colnames(norm_in)[8] = 'all'
#colnames(norm_out)[8] = 'all'
#colnames(norm_in) = colnames(norm_out)
#colnames(norm_out) = colnames(norm_in)

#shanxi_s_df = shanxi_s_df[,c(1:9)]
#linearMod <- lm(aft ~ resi + comm +  off   + bui_area , data=pop_out)  # build linear regression model on full data
#summary(linearMod)

#linearMod <- lm(mor ~ resi + comm +  off + pub + de_area + bui_area , data=pop_out)  # build linear regression model on full data
#summary(linearMod)

linearMod <- lm(all ~ resi + comm +  off  + bui_area , data=pop_out)  # build linear regression model on full data
summary(linearMod)

#linearMod <- lm(all ~ resi + comm +  off + pub + de_area + bui_area , data=pop_out)  # build linear regression model on full data
#summary(linearMod)

#linearMod <- lm(all ~ resi + comm +  off + pub + de_area + bui_area , data=norm_out)  # build linear regression model on full data
#summary(linearMod)

#linearMod <- lm(all ~ resi + comm +  off  + bui_area , data=pop_out)  # build linear regression model on full data
#summary(linearMod)
#linearMod <- lm(all ~ resi + comm + off + bui_area , data=norm_out)  # build linear regression model on full data
#summary(linearMod)
#plot(linearMod)


#linearMod <- lm(all ~ resi + comm  +bui_area +de_area, data=norm_out)  # build linear regression model on full data
#summary(linearMod)

#linearMod <- lm(all ~ resi + comm + off + pub+ de_area + bui_area, data=pop_in)  # build linear regression model on full data
#summary(linearMod)
#linearMod <- lm(all ~ resi   +comm + off +  de_area , data=pop_in)  # build linear regression model on full data
#summary(linearMod)
#regfit.best=regsubsets(all ~ resi + comm + off + pub +  bui_area + de_area, data=norm_out,nvmax=6)
#summary(regfit.best)
#regfit.best=regsubsets(all ~ resi + comm + off + pub +  bui_area +de_area, data=pop_in,nvmax=6)
#summary(regfit.best)
#plot(norm_out)
#AIC(linearMod)
vif(linearMod)

# 去掉室内口
count_feature_in_sector = function(sector_file, exit_shp, buf_distance){
  buf_near <- st_buffer(exit_shp, dist = buf_distance)
  buf_st= st_union(buf_near)
  buf_resi = st_intersection(poi_residential,buf_st)
  buf_comm = st_intersection(poi_commercial,buf_st)
  buf_off = st_intersection(poi_office,buf_st)
  buf_pub = st_intersection(poi_public,buf_st)
  buf_build = st_intersection(st_make_valid(building),buf_st)
  n = length(sector_file)
  df = as.data.frame(matrix(0,n,))
  for (i in 1:n){
    print(i)
    if (length(sector_file[[i]]) == 0){
      df$V1[i] = i
    } else {
      sector_sfc =st_sfc(sector_file[[i]],crs = st_crs(poi_residential))
      sel_resi = st_intersection(buf_resi,sector_sfc)
      sel_comm = st_intersection(buf_comm,sector_sfc)
      sel_off = st_intersection(buf_off,sector_sfc)
      sel_pub = st_intersection(buf_pub,sector_sfc)
      sel_build = st_intersection(buf_build,sector_sfc)
      df$V1[i] = i
      df$resi[i] = nrow(sel_resi)
      df$comm[i] = nrow(sel_comm)
      df$off[i] = nrow(sel_off)
      df$pub[i] = nrow(sel_pub)
      # square kilometers
      df$de_area[i] = sum(sel_build$AREA)/1000000
      df$bui_area[i] = sum(sel_build$AREA*sel_build$Height/3)/1000000
      df$mean_h[i] = mean(sel_build$Height)
    }
  }
  print("done with counting features")
  return(df)
}


# defined service area  near the station
count_feature_in_sector = function(sector_file, exit_shp, buf_distance,service_area){
  buf_st= service_area
  buf_resi = st_intersection(poi_residential,buf_st)
  buf_comm = st_intersection(poi_commercial,buf_st)
  buf_off = st_intersection(poi_office,buf_st)
  buf_pub = st_intersection(poi_public,buf_st)
  buf_build = st_intersection(st_make_valid(building),buf_st)
  n = length(sector_file)
  df = as.data.frame(matrix(0,n,))
  for (i in 1:n){
    print(i)
    if (length(sector_file[[i]]) == 0){
      df$V1[i] = i
    } else {
      sector_sfc =st_sfc(sector_file[[i]],crs = st_crs(poi_residential))
      sel_resi = st_intersection(buf_resi,sector_sfc)
      sel_comm = st_intersection(buf_comm,sector_sfc)
      sel_off = st_intersection(buf_off,sector_sfc)
      sel_pub = st_intersection(buf_pub,sector_sfc)
      sel_build = st_intersection(buf_build,sector_sfc)
      df$V1[i] = i
      df$resi[i] = nrow(sel_resi)
      df$comm[i] = nrow(sel_comm)
      df$off[i] = nrow(sel_off)
      df$pub[i] = nrow(sel_pub)
      # square kilometers
      df$de_area[i] = sum(sel_build$AREA)/1000000
      df$bui_area[i] = sum(sel_build$AREA*sel_build$Height/3)/1000000
      df$mean_h[i] = mean(sel_build$Height)
    }
  }
  print("done with counting features")
  return(df)
}


point_decay_df= function(single_exit_shp,poi_file){
  df = poi_file
  df$dist_matrix <- as.numeric(st_distance(single_exit_shp$geometry,df$geometry)[,1:nrow(df)])
  df$poi_decay <-  exp(-1/2*(df$dist_matrix/1500)^2)
  return(df)
}

poly_decay_df= function(single_exit_shp,bui_file){
  df = bui_file
  df$dist_matrix <- as.numeric(st_distance(single_exit_shp$geometry, st_centroid(df$geometry))[,1:nrow(df)]) 
  df$decay_area <-  exp(-1/2*(df$dist_matrix/1500)^2)*df$AREA
  return(df)
}


func = 'resi'
exit_no = 2
network_decay_df= function(exit_no,poi_file, station, func){
  df = poi_file
  colnames(df)[1] = 'FID'
  dist_file = read_csv(paste0(station,"_",func,'.csv'))[,exit_no]
  colnames(dist_file) ='dist'
  dist_file$FID = c(1:nrow(dist_file))
  df <- left_join(df,dist_file,by = 'FID') 
  df$poi_decay <-  exp(-1/2*(df$dist/1500)^2)
  return(df)
}
# distance decay


count_feature_in_sector = function(sector_file, exit_shp, buf_distance,service_area){
  buf_st= service_area
  buf_resi = st_intersection(poi_residential,buf_st)
  buf_comm = st_intersection(poi_commercial,buf_st)
  buf_off = st_intersection(poi_office,buf_st)
  buf_pub = st_intersection(poi_public,buf_st)
  buf_build = st_intersection(st_make_valid(building),buf_st)
  n = length(sector_file)
  #n = 1
  df = as.data.frame(matrix(0,n,))
  for (i in 1:n){
    print(i)
    if (length(sector_file[[i]]) == 0){
      df$V1[i] = i
    } else {
      sector_sfc =st_sfc(sector_file[[i]],crs = st_crs(poi_residential))
      sel_resi = st_intersection(buf_resi,sector_sfc)
      dec_resi = point_decay_df(exit_shp[i,],sel_resi)
      sel_comm = st_intersection(buf_comm,sector_sfc)
      dec_comm = point_decay_df(exit_shp[i,],sel_comm)
      sel_off = st_intersection(buf_off,sector_sfc)
      dec_off = point_decay_df(exit_shp[i,],sel_off)
      sel_pub = st_intersection(buf_pub,sector_sfc)
      dec_pub = point_decay_df(exit_shp[i,],sel_pub)
      sel_build = st_intersection(buf_build,sector_sfc)
      dec_build = poly_decay_df(exit_shp[i,],sel_build)
      df$V1[i] = i
      df$resi[i] = sum(dec_resi$poi_decay)
      df$comm[i] = sum(dec_comm$poi_decay)
      df$off[i] =sum(dec_off$poi_decay)
      df$pub[i] = sum(dec_pub$poi_decay)
      # square kilometers
      
      df$de_area[i] = sum(dec_build$decay_area)/1000000
      df$bui_area[i] = sum(dec_build$decay_area*sel_build$Height/3)/1000000
      df$mean_h[i] = mean(sel_build$Height)
    }
  }
  print("done with counting features")
  return(df)
}

#network decay
station = "shanxi"
sector_file = shanxi_s_sector
i = 2
count_feature_in_sector = function(sector_file, exit_shp, buf_distance,station){
  resi_shp = st_read(paste0(station,'_resi.shp'))
  resi_shp[,1] = as.numeric(rownames(resi_shp))
  off_shp = st_read(paste0(station,'_off.shp'))
  off_shp[,1] = as.numeric(rownames(off_shp))
  pub_shp = st_read(paste0(station,'_pub.shp'))
  pub_shp[,1] = as.numeric(rownames(pub_shp))
  comm_shp = st_read(paste0(station,'_comm.shp'))
  comm_shp[,1] = as.numeric(rownames(comm_shp))
  build_shp = st_read(paste0(station,'_centr.shp'))
  build_shp[,1] = as.numeric(rownames(build_shp))
  n = length(sector_file)
  #n = 1
  df = as.data.frame(matrix(0,n,))
  for (i in 1:n){
    print(i)
    if (length(sector_file[[i]]) == 0){
      df$V1[i] = i
    } else {
      sector_sfc =st_sfc(sector_file[[i]],crs = st_crs(resi_shp))
      sel_resi = st_intersection(resi_shp,sector_sfc)
      dec_resi = network_decay_df(i,sel_resi,station,'resi')
      sel_comm = st_intersection(comm_shp,sector_sfc)
      dec_comm = network_decay_df(i,sel_comm,station,'comm')
      sel_off = st_intersection(off_shp,sector_sfc)
      dec_off = network_decay_df(i,sel_off,station,'off')
      sel_pub = st_intersection(pub_shp,sector_sfc)
      dec_pub = network_decay_df(i,sel_pub,station,'pub')
      sel_build = st_intersection(build_shp,sector_sfc)
      dec_build = network_decay_df(i,sel_build,station,'centr')
      df$V1[i] = i
      df$resi[i] = sum(dec_resi$poi_decay)
      df$comm[i] = sum(dec_comm$poi_decay)
      df$off[i] =sum(dec_off$poi_decay)
      df$pub[i] = sum(dec_pub$poi_decay)
      # square kilometers
      
      df$de_area[i] = sum(dec_build$AREA)/1000000
      df$bui_area[i] = sum(dec_build$AREA*sel_build$Height/3)/1000000
      df$mean_h[i] = mean(sel_build$Height)
    }
  }
  print("done with counting features")
  return(df)
}

shanxi_s_df = count_feature_in_sector(shanxi_s_sector,shanxi_exit, buf_dist,'shanxi')
shanxi_s_df$ref = as.numeric(as.character(shanxi_exit$ref))


jiangsu_s_df = count_feature_in_sector(jiangsu_s_sector,jiangsu_exit, buf_dist,'jiangsu')
jiangsu_s_df$ref = as.numeric(as.character(jiangsu_exit$ref))


renmin_s_df = count_feature_in_sector(renmin_s_sector,renmin_exit, buf_dist,'renmin')
renmin_s_df$ref = as.numeric(as.character(renmin_exit$ref))

#与整体buffer 对比




