rm(list=ls())
######### Task1 ############
library(tidyverse)
mpg %>% tbl_df
my_plot <- ggplot(data=mpg, mapping=aes(x=displ, y=hwy))
my_plot + geom_point(mapping = aes(color = trans))


######### Task2 ############
my_plot + geom_point(colour = "red") + geom_smooth()

######### Task3 ############
my_plot <- ggplot(data=mpg, mapping=aes(x=displ, y=hwy, colour= drv))
my_plot + geom_point() + geom_smooth(method = 'lm', se=FALSE)


######### Task4 ############
my_plot <- ggplot(data=mpg, mapping=aes(x=displ, y=hwy, colour= factor(cyl))
                  my_plot + geom_point() + geom_smooth()
                  
                  ######### Task5 ############
                  ggplot(mpg, aes(x=displ, y= hwy, colour = factor(cyl))) + geom_point() + geom_smooth(method="lm", se=FALSE) +
                    scale_x_log10(breaks=c(2,3,4,5,6,7)) + scale_y_log10(breaks=c(20,30,40)) +
                    labs(x = "Displacement", y= "MPG, highway", colour = "Cylinders", title="Fuel economy and engine size") + 
                    facet_wrap(~year, labeller = as_labeller(c("1999"= "Model year 1999", "2008" = "Model year 2008"))) 
                  
                  ######### Task6 ############
                  library(dplyr)
                  library(tidyverse)
                  library(tidyr)
                  data(mpg, package = "ggplot2")
                  mpg %>% tbl_df
                  
                  # select columns
                  mpg %>% select(manufacturer, model, displ, year, cyl, trans, cty, hwy)
                  mpg2 <- mpg %>% select(manufacturer, model, displ, year, cyl, trans, cty, hwy)
                  
                  ######### Task7 ############
                  mpg2 %>% mutate(displ2 = displ^2, vol_per_cyl = displ/cyl)
                  mpg3 <- mpg2 %>% mutate(displ2 = displ^2, vol_per_cyl = round(displ/cyl,2))
                  
                  ######### Task8 ############
                  mpg3 %>% arrange(desc(vol_per_cyl))
                  mpg3 %>% filter(manufacturer == "chevrolet") %>% arrange(desc(vol_per_cyl))
                  mpg3 %>% group_by(manufacturer, year) %>% summarise(max_vol_per_cyl=max(vol_per_cyl))
                  mpg4 <- mpg3 %>% group_by(manufacturer, year) %>% summarise(max_vol_per_cyl=max(vol_per_cyl))
                  
                  ######### Task9 ############
                  mpg4 %>% spread(year, max_vol_per_cyl)
                  mpg5 <- mpg4 %>% spread(year, max_vol_per_cyl)
                  
                  ######### Task10 ############
                  mpg5 %>% mutate(change = `2008` - `1999`)
                  mpg6 <- mpg5 %>% mutate(change = `2008` - `1999`)
                  
                  ######### Task11 ############
                  mpg6 %>% rename(max_vpc_1999 = `1999`, max_vpc_2008 = `2008`) %>% gather(variable, value, max_vpc_1999, max_vpc_2008, change) %>% View
                  
                  
                  ######### Task12 ############
                  install.packages("nycflights13")
                  library(tidyverse)
                  library(nycflights13)
                  flights %>% tbl_df
                  airlines %>% tbl_df
                  weather %>% tbl_df
                  flights2<- flights %>% select(origin, year, month, day, hour, sched_dep_time, dep_delay, carrier)
                  weather2<- weather %>% select(origin, year, month, day, hour, precip, wind_speed, visib)
                  inner_join(flights, airlines)
                  # carrier was used as the key for this join, Joining, by = "carrier"
                  flights2 %>% left_join(weather2)
                  #Joining, by = c("origin", "year", "month", "day", "hour")
                  #because the value under precip, wind_speed, and visib were missing from weather2
                  
                  
                  ######### Task13 ############
                  weather2 %>% summarise(min_precip= min(precip, na.rm = TRUE), min_wind = min(wind_speed, narm= TRUE), max_visib = max(visib, na.rm = TRUE))
                  good_weather_delays <- inner_join(flights2, weather2) %>% filter(precip == 0, wind_speed == 0, visib == 10)
                  #Joining, by = c("origin", "year", "month", "day", "hour")
                  good_weather_delays %>% group_by(carrier) %>% summarise(a_dep_delay = mean(dep_delay)) %>% arrange(a_dep_delay) %>% left_join(airlines)
                  #Joining, by = "carrier"
                  avg_goog_weather_delays <- good_weather_delays %>% group_by(carrier) %>% summarise(a_dep_delay = mean(dep_delay)) %>% arrange(a_dep_delay) %>% left_join(airlines)
                  
                  ######### Task14 ############
                  ranked_airline_labels <- avg_goog_weather_delays %>% transmute(carrier, name = factor(-row_number(),labels = name))
                  newdata<- ranked_airline_labels %>% left_join(good_weather_delays)
                  ggplot(data = newdata, mapping= aes(x= name, y=dep_delay)) + coord_flip() + labs(x= "Average departure delay", title= "Departure delays under ideal weather conditions NYC airports, 2013")
                  