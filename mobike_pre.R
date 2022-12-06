library(sf)
library(lubridate)


setwd("/Users/../sh_metro")
getwd()

start_od = st_read("start_OD.shp") 
end_od = st_read("end_OD.shp") 

# 直线路程超过300，速度小于8m/s, 运行时间超过60s
od_file = Start_OD
cleaned_od = function(od_file){
  od_cleaned = od_file[which(od_file$Length>300 & od_file$Length<10000 ),]
  od_cleaned =od_cleaned[which(od_cleaned$时长>60),] 
  return(od_cleaned)
}
start_od_cleaned = start_od[which(start_od$Length>300),]
start_od_cleaned  = start_od_cleaned[which(start_od_cleaned$speed<8),]
start_od_cleaned  = start_od_cleaned[which(start_od_cleaned$时长_分>1),]

shanxinan_s_od  = start_od_cleaned[which(start_od_cleaned$NAME == "陕西南路"),]


end_od_cleaned = end_od[which(end_od$Length>300),]
end_od_cleaned  = end_od_cleaned[which(end_od_cleaned$speed<8),]

seconds_to_period(48000)

hist(start_od_cleaned$Length,breaks = 80)
hist(end_od_cleaned$Length, breaks = 80)
hist(start_od_cleaned$时长_分,breaks = 200)

shanxinan_s_od$secon_s = seconds_to_period(shanxinan_s_od$secon_s)
shanxinan_s_od$secon_e = seconds_to_period(shanxinan_s_od$secon_e)


write.csv(shanxinan_s_od$OBJECTID_1,"shanxinan.csv")
write.csv(start_od,"start_od.csv")
