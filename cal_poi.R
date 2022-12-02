library(sp)
library(rgeos)
library(sf)
library(lwgeom)
library(tmap)
setwd("/Users/liumengyang/OneDrive/sh_metro")
getwd()

angle = 120
Start_OD = st_read("shanxinan_s_od.shp") 
Start_OD = cleaned_od(Start_OD)
#End_OD = st_read("shanxinan_e_od.shp") 
#End_OD = cleaned_od(End_OD)
shanxi_exit = st_read('shanxinan_exit.shp')


shanxi_s_sector  = start_od_sel_sector(Start_OD,shanxi_exit,angle)
#shanxi_e_sector  = end_od_sel_sector(End_OD,shanxi_exit,angle)

Start_OD = st_read("renmin_s_od.shp") 
Start_OD = cleaned_od(Start_OD)
#End_OD = st_read("renmin_e_od.shp")
#End_OD = cleaned_od(End_OD)

renmin_exit = st_read('renmin_exit.shp')
renmin_exit = renmin_exit[c(-2,-9),]
renmin_s_sector  = start_od_sel_sector(Start_OD,renmin_exit,angle)
#renmin_e_sector  = end_od_sel_sector(End_OD,renmin_exit,angle)

Start_OD = st_read("jiangsu_s_od.shp") 
Start_OD = cleaned_od(Start_OD)
#End_OD = st_read("jiangsu_e_od.shp") 
#End_OD = cleaned_od(End_OD)


jiangsu_exit = st_read('jiangsu_exit.shp')


#End_OD = cleaned_od(End_OD)
jiangsu_s_sector  = start_od_sel_sector(Start_OD,jiangsu_exit,angle)
#jiangsu_e_sector  = end_od_sel_sector(End_OD,jiangsu_exit,angle)



tm_shape(renmin_exit$geometry, bbox = bbox_new)+ tm_sf(col = "red") + tm_shape(renmin_s_sector[[2]])+ tm_polygons(alpha = 0.3)+
  tm_shape(renmin_s_sector[[3]])+ tm_polygons(alpha = 0.3)+tm_shape(renmin_s_sector[[4]])+ tm_polygons(alpha = 0.3)+
  tm_shape(renmin_s_sector[[5]])+ tm_polygons(alpha = 0.3)+tm_shape(renmin_s_sector[[7]])+ tm_polygons(alpha = 0.3)+
  tm_shape(renmin_s_sector[[9]])+ tm_polygons(alpha = 0.3)+tm_shape(renmin_s_sector[[11]])+ tm_polygons(alpha = 0.3)+
  tm_shape(renmin_s_sector[[12]])+ tm_polygons(alpha = 0.3)+tm_shape(renmin_s_sector[[13]])+ tm_polygons(alpha = 0.3)+
  tm_shape(renmin_s_sector[[14]])+ tm_polygons(alpha = 0.3)+tm_shape(renmin_s_sector[[15]])+ tm_polygons(alpha = 0.3)+
  tm_shape(renmin_s_sector[[17]])+ tm_polygons(alpha = 0.3)+tm_shape(renmin_s_sector[[18]])+ tm_polygons(alpha = 0.3)

shanxi_s_sector[[1]] = st_sfc(shanxi_s_sector[[1]],crs = st_crs(shanxi_s_sector[[2]]))
tm_shape(shanxi_area$geometry) + tm_polygons(col = "lightblue",alpha = 0.5) + tm_shape(shanxi_exit$geometry, bbox = bbox_new)+ tm_sf(col = "red", size = 0.5) + tm_shape(shanxi_s_sector[[1]])+ tm_polygons(alpha = 0.3)+
  tm_shape(shanxi_s_sector[[2]])+ tm_polygons(alpha = 0.3)+tm_shape(shanxi_s_sector[[3]])+ tm_polygons(alpha = 0.3)+
  tm_shape(shanxi_s_sector[[4]])+ tm_polygons(alpha = 0.3)+tm_shape(shanxi_s_sector[[5]])+ tm_polygons(alpha = 0.3)+
  tm_shape(shanxi_s_sector[[6]])+ tm_polygons(alpha = 0.3)+tm_shape(shanxi_s_sector[[7]])+ tm_polygons(alpha = 0.3)+
  tm_shape(shanxi_s_sector[[8]])+ tm_polygons(alpha = 0.3)+tm_shape(shanxi_s_sector[[9]])+ tm_polygons(alpha = 0.3)

  
#plot(shanxi_s_sector[[2]])

bbox_new <- st_bbox(shanxi_exit)
xrange = 1500
yrange = 1000
bbox_new[1] <- bbox_new[1] - (0.5 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.5 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.5 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc()



# only start
start_od_sel_sector = function(start_od_file, exit_file, sector_angle){
  # create buffer
  buf_far <- st_buffer(exit_file,  dist = 21000 )
  
  start_l = start_list(start_od_file)
  end_l = end_list(start_od_file)
  
  start_point = st_multipoint(start_l)
  plot(start_point)
  end_point = st_multipoint(end_l)
  plot(end_point)
  end_sfc =st_sfc(end_point,crs = st_crs(end_point))
  end_p = st_cast(end_sfc,"POINT")
  
  # get the nearest start point/od
  start_sfc =st_sfc(start_point,crs = st_crs(exit_file))
  # seperate the point from multipoint
  start_p = st_cast(start_sfc,"POINT")
  # creates a matrix of distances
  dist_matrix   <- st_distance(start_p,exit_file)  
  start_near = as.data.frame(matrix(1,nrow(dist_matrix),3))
  colnames(start_near) = c('id',"gate_id","dist")
  for (i in 1:nrow(dist_matrix)){
    start_near[i,1] = i
    start_near[i,2] = which.min(dist_matrix[i,])
    start_near[i,3] = dist_matrix[i,which.min(dist_matrix[i,])]
  }
  sector_list = list()
  # nrow(buf_far)
  for (j in 1:nrow(buf_far)){
    print(j)
    if (length(which(start_near$gate_id == j))<2){
      print("gate do not have enough od")
      sector_list[[j]] =st_polygon()
    } else{
      buffer_shp = buf_far$geometry[j]
      end_sel = end_p[which(start_near$gate_id == j),]
      end_com = st_union(end_sel)
      max_sector = sel_sector(buffer_shp, sector_angle, end_com)
      max = which.max(unlist(max_sector))
      print(unlist(max_sector))
      print("max")
      print(max)
      final_sector = plot_sector (buffer_shp,max,sector_angle)
      plot(final_sector)
      if (j == 1){
        sector_list = final_sector
      } else {
        sector_list[[j]] = final_sector
      }
      print("done")
      print(j)
    }
  }
  return(sector_list)
}
end_od_sel_sector = function(end_od_file, exit_file, sector_angle){
  # create buffer
  buf_far <- st_buffer(exit_file,  dist = 21000 )
  
  start_l = end_list(end_od_file)
  end_l = start_list(end_od_file)
  
  start_point = st_multipoint(start_l)
  plot(start_point)
  end_point = st_multipoint(end_l)
  plot(end_point)
  end_sfc =st_sfc(end_point,crs = st_crs(end_point))
  end_p = st_cast(end_sfc,"POINT")
  
  # get the nearest start point/od
  start_sfc =st_sfc(start_point,crs = st_crs(exit_file))
  # seperate the point from multipoint
  start_p = st_cast(start_sfc,"POINT")
  # creates a matrix of distances
  dist_matrix   <- st_distance(start_p,exit_file)  
  start_near = as.data.frame(matrix(1,nrow(dist_matrix),3))
  colnames(start_near) = c('id',"gate_id","dist")
  for (i in 1:nrow(dist_matrix)){
    start_near[i,1] = i
    start_near[i,2] = which.min(dist_matrix[i,])
    start_near[i,3] = dist_matrix[i,which.min(dist_matrix[i,])]
  }
  sector_list = list()
  # nrow(buf_far)
  for (j in 1:nrow(buf_far)){
    print(j)
    if (length(which(start_near$gate_id == j)) == 0){
      print("gate do not have od")
      sector_list[[j]] =st_polygon()
    } else{
      buffer_shp = buf_far$geometry[j]
      end_sel = end_p[which(start_near$gate_id == j),]
      end_com = st_union(end_sel)
      max_sector = sel_sector(buffer_shp, sector_angle, end_com)
      max = which.max(unlist(max_sector))
      print(unlist(max_sector))
      print("max")
      print(max)
      final_sector = plot_sector (buffer_shp,max,sector_angle)
      plot(final_sector)
      if (j == 1){
        sector_list =  final_sector
      } else {
        sector_list[[j]] = final_sector
      }
      print("done")
      print(j)
    }
  }
  return(sector_list)
}

# get the start/end point for each exit

point_list_release = function(point_file){
  list = matrix(1,nrow(point_file),2)
  for (i in 1:nrow(point_file)){
    print(i)
    ptns = st_cast(point_file$geometry[i], "POINT")
    p = ptns[[1]]
    list[i,1] = p[1]
    list[i,2] = p[2]
  }
  return(list)  
}


start_list = function(od_shp){
  start_list = matrix(1,nrow(od_shp),2)
  for (i in 1:nrow(od_shp)){
    print(i)
    ptns = st_cast(od_shp$geometry[i], "POINT")
    start_p = ptns[[1]]
    start_list[i,1] = start_p[1]
    start_list[i,2] = start_p[2]
  }
  return(start_list)
}
end_list = function(od_shp){
  end_list = matrix(1,nrow(od_shp),2)
  for (i in 1:nrow(od_shp)){
    print(i)
    ptns = st_cast(od_shp$geometry[i], "POINT")
    end_p = ptns[[2]]
    end_list[i,1] = end_p[1]
    end_list[i,2] = end_p[2]
  }
  return(end_list)
}



#plot(end_point[1:10,])


# seprate the buffer
#1-61 11-71 21-81 31-91 41-101 51-111 61-121
#n ~ 4-360  能整除3， 能被360 整除的数
#polygon_split  = function(buffer_shp, n){
  buf_cen = st_centroid(buffer_shp)[[1]]
  inter= 360/n
  start_angle = 0
  list_line = list()
  while (start_angle < 360) {
    print(start_angle)
    a1 = buf_cen
    a2 = buffer_shp[[1]][[1]][start_angle/3+1,]
    line = rbind(a1, a2)
    print(line)
    if (length(list_line) == 0){
      list_line = list(line)
    }
    else{
      list_line = rbind(list_line, list(line))
    }
    start_angle = start_angle + inter
  }
  # create multilinestring
  mls <- st_multilinestring(list_line)
  print("line created")
  buf_split = st_split(buffer_shp, mls)[[1]]
  return(buf_split)
}

#buf_split = polygon_split(buf_far$geometry[1], 60)

#select the sub_buffer1
#cal_point_in_quad = function(buffer_split_file, n, threshold){
  buf_count = matrix(1,length(buffer_split_file) ,2)
  for (i in 1:n){
    print(i)
    buf_intersect=st_intersection(buffer_split_file[[i]], end_point)
    # count the point
    buf_count[i,1] = i
    if (is.null(nrow(buf_intersect))){
      buf_count[i,2] = 0
    }
    else{
      buf_count[i,2] = nrow(buf_intersect)
    }
  }
  # sorting
  buf_count = as.data.frame(buf_count)
  buf_sorted = order(buf_count$V2, decreasing = TRUE)
  # the percentage
  percen = 0
  for (j in as.list(buf_sorted)){
    print("sub_buffer")
    print(j)
    percen = percen + buf_count$V2[j]/nrow(end_point)
    print("percentage")
    print(percen)
    # combine the area
    if (which(buf_sorted == j) == 1){
      buf_union = buffer_split_file[[j]]
    }
    else{
      buf_union = st_union(buf_union, buffer_split_file[[j]])
    }
    if (percen > threshold){
      break
    }
  }
  return(buf_union)
}
#cal_area = cal_point_in_quad(buf_split, 60 , 0.75)
#plot(cal_area)

#select the sub_buffer2
#sel_point_in_quad = function(buffer_split_file,n,topN){
  buf_count = matrix(1,length(buffer_split_file) ,2)
  for (i in 1:n){
    print(i)
    buf_intersect=st_intersection(buffer_split_file[[i]], end_point)
    # count the point
    buf_count[i,1] = i
    if (is.null(nrow(buf_intersect))){
      buf_count[i,2] = 0
    }
    else{
      buf_count[i,2] = nrow(buf_intersect)
    }
  }
  # sorting
  buf_count = as.data.frame(buf_count)
  buf_sorted = order(buf_count$V2, decreasing = TRUE)
  # the percentage
  percen = 0
  for (j in as.list(buf_sorted[1:topN])){
    print("sub_buffer")
    print(j)
    percen = percen + buf_count$V2[j]/nrow(end_point)
    print("percentage")
    print(percen)
    # combine the area
    if (which(buf_sorted == j) == 1){
      buf_union = buffer_split_file[[j]]
    }
    else{
      buf_union = st_union(buf_union,buffer_split_file[[j]])
    }
  }
  return(buf_union)
}
#sel_area = sel_point_in_quad(buf_split, 60 , 30)
#plot(sel_area)

# 选出一个sector拥有最多od
# angle是 3 的倍数且小于180
sel_sector  = function(buffer_shp, angle, end_point){
  buf_cen = st_centroid(buffer_shp)[[1]]
  span= angle/3
  od_list = list()
  for (i in 1:120){
    print(i)
    a1 = buf_cen
    a2 = buffer_shp[[1]][[1]][i,]
    if (i + span <= 120){
      a3 = buffer_shp[[1]][[1]][i+span,]
    } else {
      a3 = buffer_shp[[1]][[1]][i+span-120,]
    }
    list_line = list(rbind(a1, a2),rbind(a1, a3))
    print(list_line) 
    mls <- st_multilinestring(list_line)
    print("line created")
    buf_split = st_split(buffer_shp, mls)[[1]]
    buf_area_list = list()
    for (j in 1:2){
      buf_area = st_area(buf_split[[j]])
      buf_area_list[j] = buf_area
    }
    k = which.min(buf_area_list)
    buf_sel = buf_split[[k]]
    print(k)
    buf_intersect=st_intersection(buf_sel, end_point)
    od_list[i] = nrow(buf_intersect)
  }
  return(od_list)
}

plot_sector  = function(buffer_shp, i, angle){
  buf_cen = st_centroid(buffer_shp)[[1]]
  span= angle/3
  od_list = list()
  a1 = buf_cen
  a2 = buffer_shp[[1]][[1]][i,]
  if (i + span <= 120){
    a3 = buffer_shp[[1]][[1]][i+span,]
  } else {
    a3 = buffer_shp[[1]][[1]][i+span-120,]
  }
  list_line = list(rbind(a1, a2),rbind(a1, a3))
  print(list_line) 
  mls <- st_multilinestring(list_line)
  print("line created")
  buf_split = st_split(buffer_shp, mls)[[1]]
  buf_area_list = list()
  for (j in 1:2){
    buf_area = st_area(buf_split[[j]])
    buf_area_list[j] = buf_area
  }
  k = which.min(buf_area_list)
  buf_sel = st_sfc(buf_split[[k]],crs = st_crs(shanxi_exit) )
  return(buf_sel)
}



