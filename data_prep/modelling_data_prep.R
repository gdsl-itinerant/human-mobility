library(tidyverse)
library(doParallel)
library(foreach)

files_p<-data.frame(list.files("~/itinerant/facebook_allBritain/modelling_data/population",full.names = T))
names(files_p)<-"files_p"
files_p$check<-substr(files_p$files_p,88,102)
files_m<-data.frame(list.files("~/itinerant/facebook_allBritain/modelling_data/movements",full.names = T))
names(files_m)<-"files_m"
files_m$check<-substr(files_m$files_m,86,100)


checkfp<-list.files("~/itinerant/facebook_allBritain/modelling_data/population",full.names = F)
checkfp<-substr(checkfp,18,32)

checkfm<-list.files("~/itinerant/facebook_allBritain/modelling_data/movements",full.names = F)
checkfm<-substr(checkfm,17,31)

check<-data.frame(setdiff(checkfm,checkfp))
names(check)<-"check_date"
check<-check %>%
  separate(check_date, c("date", "hour"), "_") %>%
  mutate(
    lagged_hour=case_when(
      hour=="0800"~"1600",
      hour=="1600"~"0000",
      hour=="0000"~"0800"
    ),
    date=ymd(date),
    lagged_date=case_when(hour=="1600" ~ date + 1,
                       hour!="1600" ~ date)
    )

files_m<-files_m[!files_m$check %in% paste0(check$lagged_date,"_",check$lagged_hour),]

check<-setdiff(checkfp,checkfm)

files_p<-files_p[!files_p$check %in% "2020-08-05_0800",]

files_p<-files_p$files_p
files_m<-files_m$files_m

##load grid with classes
grid<-st_read("~/itinerant/grid_final/grid_w_classes.gpkg")

cl <- parallel::makeCluster(27)
doParallel::registerDoParallel(cl)

foreach(i = seq(1:length(files_p)-1),.packages = c("tidyverse","lubridate","sf"))%dopar% {

pop<-read_csv(files_p[i], col_types = cols(date_time = col_character(),quadkey= col_character()))
mov<-read_csv(files_m[i+1], col_types = cols(date_time = col_character(),start_quadkey= col_character(),end_quadkey= col_character()))

pop<-pop %>%
  filter(!is.na(n_crisis) & country=="GB") %>%
  mutate(
    quadkey=substr(quadkey,0,nchar(quadkey)-1),
    date_time=mov[1,]$date_time)%>%
  group_by(quadkey,date_time)%>%
  summarise(pop=sum(n_crisis),
            pop_baseline=sum(n_baseline))


mov<-mov %>%
  filter(country=="GB" & !is.na(n_crisis)) %>%
  mutate(
    hour=substr(date_time,nchar(date_time)-5,nchar(date_time)-3),
    date=as.Date(strftime(date_time, "%Y-%m-%d", tz="GMT")),
    wday=wday(date_time,label = T),
    wd=ifelse(wday=="Sun"|wday=="Sat","Weekend","Weekday"))


mov_pop<-mov %>%
  full_join(pop,by=c("start_quadkey"="quadkey","date_time"="date_time"))%>%
  rename(
    start_pop=pop,
    start_pop_baseline=pop_baseline
  ) %>%
  inner_join(st_drop_geometry(grid[c("Population","quadkey","decile","jenks","class")]),by=c("start_quadkey"="quadkey"))%>%
  rename(
    origin_res_pop=Population,
    origin_jenks=jenks,
    origin_class=class,
    origin_decile=decile
  )%>%
  inner_join(st_drop_geometry(grid[c("quadkey","Population","decile","jenks","class")]),by=c("end_quadkey"="quadkey"))%>%
  rename(
    destination_res_pop=Population,
    destination_jenks=jenks,
    destination_class=class,
    destination_decile=decile
  )
dt<-gsub("[[:space:]]", "_",mov[1,]$date_time)
dt<-gsub(":", "",dt)        
file_name<-paste0("~/itinerant/pre-processed-movements/modelling/full_period/movpop_",dt,".rds")
saveRDS(mov_pop,file_name)

}

###for exploration

foreach(i = seq(1:length(files_p)-1),.packages = c("tidyverse","lubridate","sf"))%dopar% {
  
  pop<-read_csv(files_p[i], col_types = cols(date_time = col_character(),quadkey= col_character()))
  mov<-read_csv(files_m[i+1], col_types = cols(date_time = col_character(),start_quadkey= col_character(),end_quadkey= col_character()))
  
  pop<-pop %>%
    filter(!is.na(n_crisis) & country=="GB") %>%
    mutate(
      quadkey=substr(quadkey,0,nchar(quadkey)-1),
      date_time=mov[1,]$date_time)%>%
    group_by(quadkey,date_time)%>%
    summarise(pop=sum(n_crisis),
              pop_baseline=sum(n_baseline))
  
  
  mov<-mov %>%
    filter(country=="GB" & !is.na(n_crisis)) %>%
    mutate(
      hour=substr(date_time,nchar(date_time)-5,nchar(date_time)-3),
      date=as.Date(strftime(date_time, "%Y-%m-%d", tz="GMT")),
      wday=wday(date_time,label = T),
      wd=ifelse(wday=="Sun"|wday=="Sat","Weekend","Weekday"))
  
  
  mov_pop<-mov %>%
    full_join(pop,by=c("start_quadkey"="quadkey","date_time"="date_time"))%>%
    rename(
      start_pop=pop,
      start_pop_baseline=pop_baseline
    ) %>%
    inner_join(st_drop_geometry(grid[c("Population","quadkey","decile","jenks","class")]),by=c("start_quadkey"="quadkey"))%>%
    rename(
      origin_res_pop=Population,
      origin_jenks=jenks,
      origin_class=class,
      origin_decile=decile
    )%>%
    inner_join(st_drop_geometry(grid[c("quadkey","Population","decile","jenks","class")]),by=c("end_quadkey"="quadkey"))%>%
    rename(
      destination_res_pop=Population,
      destination_jenks=jenks,
      destination_class=class,
      destination_decile=decile
    )
  
  mov_pop<-mov_pop %>%
    group_by(start_polygon_name,origin_class,end_polygon_name,destination_class,date,hour,wday,wd)%>%
    summarise(
      n_crisis=sum(n_crisis,na.rm = T),
      n_baseline=sum(n_baseline,na.rm = T),
      start_pop=sum(start_pop,na.rm = T),
      start_pop_baseline=sum(start_pop_baseline,na.rm = T),
      origin_res_pop=sum(origin_res_pop,na.rm = T),
      destination_res_pop=sum(destination_res_pop,na.rm = T),
      length_km=mean(length_km,na.rm=T)
    )
    
    
  dt<-gsub("[[:space:]]", "_",mov[1,]$date_time)
  dt<-gsub(":", "",dt)        
  file_name<-paste0("~/itinerant/pre-processed-movements/exploration/full_period/mov_pop_",dt,".rds")
  saveRDS(mov_pop,file_name)
  
}
