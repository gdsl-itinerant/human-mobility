library(tidyverse)
library(ggpubr)
library(ggthemes)


##Load and pre-process the data

df <- readRDS("~/Projects/ITINERANT/full_period_mov_pop.rds") %>% 
  select( -c(tile_size, start_polygon_id, end_polygon_id, country, level, is_statistically_significant, start_quadkey, end_quadkey, n_baseline, n_difference, percent_change, start_lat, start_lon, end_lat, end_lon, geometry) )
df <- df %>% mutate(
  mov_prop = n_crisis / start_pop
) %>% 
  filter(mov_prop <= 1)

df <-  df %>% mutate(
  origin_class = ordered(origin_class, 
                         levels = c("1","2","3","4","5","6","7","8","9","10")),
  destination_class = ordered(destination_class, 
                              levels = c("1","2","3","4","5","6","7","8","9","10")),
  wd = factor(wd,
              levels = c("Weekday", "Weekend"))
)
df$origin_destination_class <- fct_cross(df$origin_class, df$destination_class)
df$day <- as.numeric(ordered(df$date))
df$month <- lubridate::month(lubridate::ymd(df$date), label=TRUE)
month <- unique(df$month)
df$monthyear = lubridate::floor_date(df$date, "month")
monthyear <- unique(df$monthyear)

#Get descriptive stats for each month

descriptive_montly<-NULL

for (i in 1:18){
  print(monthyear[i])
  dft<-df[df$monthyear==monthyear[i],]
  print(summary(dft$month))
  dft<-dft %>%
    group_by(origin_destination_class,monthyear)%>%
    summarise(
      avg=mean(n_crisis),
      sum=sum(n_crisis),
      nobs=n()
    )
  descriptive_montly<-rbind(descriptive_montly,dft)
}

descriptive_montly<-descriptive_montly %>%
  separate(origin_destination_class, c("origin_class","destination_class"), sep = "([:])") %>%
  mutate(
    origin_destination_class=paste0(origin_class,"_",destination_class),
    origin_class = ordered(origin_class, 
                           levels = c("1","2","3","4","5","6","7","8","9","10")),
    destination_class = ordered(destination_class, 
                                levels = c("1","2","3","4","5","6","7","8","9","10")
    ))

g<-ggplot(descriptive_montly,aes(x=monthyear,y=sum))+
  #geom_point()+
  facet_grid(origin_class ~ destination_class) +
  geom_hline( yintercept = 0, linetype = "solid", colour = "gray85", size = 0.5) +
  geom_vline( xintercept = as.numeric(as.Date("2020-03-23")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-03-08")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-04-12")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-07-19")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_line(colour = "darkblue", size = 1) + 
  theme_tufte() +
  #theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none",
        text=element_text(size=12),
        plot.subtitle = element_text(color = c('black'), hjust = 0.5, size = 12,face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45),
        axis.title=element_text(size=12)
  ) +
  labs(x= "Month",
       y = "Flow Sum",
       subtitle = "Destination Class") +
  scale_x_date(date_labels = "%b",
               date_breaks = "3 months", #breaks = c("Mar", "Jun", "Sep", "Dec"),
               limits = c(as.Date("2020-01-01"), as.Date("2021-08-01"))
  )

png("~/monthyear_exploration_sum.png",units="in", width=10, height=10, res=300)
annotate_figure(g, 
                left = text_grob("Origin Class", 
                                 face= "bold", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "serif"))
dev.off()

g1<-ggplot(descriptive_montly[descriptive_montly$origin_class!=descriptive_montly$destination_class,],aes(x=monthyear,y=sum))+
  #geom_point()+
  facet_grid(origin_class ~ destination_class) +
  geom_hline( yintercept = 0, linetype = "solid", colour = "gray85", size = 0.5) +
  geom_vline( xintercept = as.numeric(as.Date("2020-03-23")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-03-08")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-04-12")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-07-19")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_line(colour = "darkblue", size = 1) + 
  theme_tufte() +
  #theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none",
        text=element_text(size=12),
        plot.subtitle = element_text(color = c('black'), hjust = 0.5, size = 12,face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45),
        axis.title=element_text(size=12)
  ) +
  labs(x= "Month",
       y = "Flow Sum",
       subtitle = "Destination Class") +
  scale_x_date(date_labels = "%b",
               date_breaks = "3 months", #breaks = c("Mar", "Jun", "Sep", "Dec"),
               limits = c(as.Date("2020-01-01"), as.Date("2021-08-01"))
  )

png("~/monthyear_exploration_sum_nd.png",units="in", width=10, height=10, res=300)
annotate_figure(g1, 
                left = text_grob("Origin Class", 
                                 face= "bold", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "serif"))
dev.off()

g2<-ggplot(descriptive_montly,aes(x=monthyear,y=avg))+
  #geom_point()+
  facet_grid(origin_class ~ destination_class) +
  geom_hline( yintercept = 0, linetype = "solid", colour = "gray85", size = 0.5) +
  geom_vline( xintercept = as.numeric(as.Date("2020-03-23")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-03-08")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-04-12")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-07-19")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_line(colour = "darkblue", size = 1) + 
  theme_tufte() +
  #theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none",
        text=element_text(size=12),
        plot.subtitle = element_text(color = c('black'), hjust = 0.5, size = 12,face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45),
        axis.title=element_text(size=12)
  ) +
  labs(x= "Month",
       y = "Flow Avg",
       subtitle = "Destination Class") +
  scale_x_date(date_labels = "%b",
               date_breaks = "3 months", #breaks = c("Mar", "Jun", "Sep", "Dec"),
               limits = c(as.Date("2020-01-01"), as.Date("2021-08-01"))
  )

png("~/monthyear_exploration_avg.png",units="in", width=10, height=10, res=300)
annotate_figure(g2, 
                left = text_grob("Origin Class", 
                                 face= "bold", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "serif"))
dev.off()

g3<-ggplot(descriptive_montly[descriptive_montly$origin_class!=descriptive_montly$destination_class,],aes(x=monthyear,y=avg))+
  #geom_point()+
  facet_grid(origin_class ~ destination_class) +
  geom_hline( yintercept = 0, linetype = "solid", colour = "gray85", size = 0.5) +
  geom_vline( xintercept = as.numeric(as.Date("2020-03-23")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-03-08")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-04-12")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-07-19")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_line(colour = "darkblue", size = 1) + 
  theme_tufte() +
  #theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none",
        text=element_text(size=12),
        plot.subtitle = element_text(color = c('black'), hjust = 0.5, size = 12,face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45),
        axis.title=element_text(size=12)
  ) +
  labs(x= "Month",
       y = "Flow Avg",
       subtitle = "Destination Class") +
  scale_x_date(date_labels = "%b",
               date_breaks = "3 months", #breaks = c("Mar", "Jun", "Sep", "Dec"),
               limits = c(as.Date("2020-01-01"), as.Date("2021-08-01"))
  )

png("~/monthyear_exploration_avg_nd.png",units="in", width=10, height=10, res=300)
annotate_figure(g3, 
                left = text_grob("Origin Class", 
                                 face= "bold", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "serif"))
dev.off()

g4<-ggplot(descriptive_montly,aes(x=monthyear,y=nobs))+
  #geom_point()+
  facet_grid(origin_class ~ destination_class) +
  geom_hline( yintercept = 0, linetype = "solid", colour = "gray85", size = 0.5) +
  geom_vline( xintercept = as.numeric(as.Date("2020-03-23")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-03-08")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-04-12")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-07-19")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_line(colour = "darkblue", size = 1) + 
  theme_tufte() +
  #theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none",
        text=element_text(size=12),
        plot.subtitle = element_text(color = c('black'), hjust = 0.5, size = 12,face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45),
        axis.title=element_text(size=12)
  ) +
  labs(x= "Month",
       y = "N Obs.",
       subtitle = "Destination Class") +
  scale_x_date(date_labels = "%b",
               date_breaks = "3 months", #breaks = c("Mar", "Jun", "Sep", "Dec"),
               limits = c(as.Date("2020-01-01"), as.Date("2021-08-01"))
  )

png("~/monthyear_exploration_obs.png",units="in", width=10, height=10, res=300)
annotate_figure(g4, 
                left = text_grob("Origin Class", 
                                 face= "bold", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "serif"))
dev.off()

g5<-ggplot(descriptive_montly[descriptive_montly$origin_class!=descriptive_montly$destination_class,],aes(x=monthyear,y=nobs))+
  #geom_point()+
  facet_grid(origin_class ~ destination_class) +
  geom_hline( yintercept = 0, linetype = "solid", colour = "gray85", size = 0.5) +
  geom_vline( xintercept = as.numeric(as.Date("2020-03-23")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-03-08")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-04-12")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_vline( xintercept = as.numeric(as.Date("2021-07-19")), linetype = "solid", colour = "gray85", size = 0.2) +
  geom_line(colour = "darkblue", size = 1) + 
  theme_tufte() +
  #theme(text = element_text(family="robotocondensed")) +
  theme(legend.position = "none",
        text=element_text(size=12),
        plot.subtitle = element_text(color = c('black'), hjust = 0.5, size = 12,face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45),
        axis.title=element_text(size=12)
  ) +
  labs(x= "Month",
       y = "N Obs.",
       subtitle = "Destination Class") +
  scale_x_date(date_labels = "%b",
               date_breaks = "3 months", #breaks = c("Mar", "Jun", "Sep", "Dec"),
               limits = c(as.Date("2020-01-01"), as.Date("2021-08-01"))
  )

png("~/monthyear_exploration_obs_nd.png",units="in", width=10, height=10, res=300)
annotate_figure(g5, 
                left = text_grob("Origin Class", 
                                 face= "bold", 
                                 color = "black", 
                                 rot = 90, 
                                 size = 12,
                                 family = "serif"))
dev.off()
