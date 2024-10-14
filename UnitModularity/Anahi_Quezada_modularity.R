setwd("C:/Users/asque/Documents/ML/QuezadaMLbiol/UnitModularity")
#Packages
install.packages("readRDS")
library(readRDS)
library(maps)
library(dplyr)
library(ggplot2)
#Data
precip <- readRDS(file = "USAAnnualPcpn1950_2008.rds")
temp <- readRDS(file="USAAnnualTemp1950_2008.rds")

#Precipitation
precip_clean <- na.omit(precip)
#at least 40 
prec_40 <- precip_clean %>%
  group_by(state, name, lon, lat) %>%
  filter(n()>=40)%>%
  ungroup()

#function 
slope_p <- function(df){
  df <- na.omit(df)
  if (nrow(df)<2){
    return(NA)
  }
  model <- lm(data ~ year, data = df)
  return(coef(model)["year"])
}
slopes_p <- prec_40 %>%
  group_by(state, name, lon, lat) %>%
  summarize(
    slope_p_1 = slope_p(cur_data())
  )

print(slopes_p)

#Temperatura
precip_temp <- na.omit(temp)

#at least 40 
temp_40 <- precip_temp %>%
  group_by(state, name, lon, lat) %>%
  filter(n()>=40) %>%
  ungroup()
#function 
slope_t <- function(df){
  df <- na.omit(df)
  if (nrow(df)<2){
    return(NA)
  }
  model <- lm(data ~ year, data = df)
  return(coef(model)["year"])
}
slopes_t <- temp_40 %>%
  group_by(state, name, lon, lat) %>%
  summarize(
    slope_t_1 = slope_t(cur_data())
  )

print(slopes_t)

#histogram 
hist(slopes_t$slope_t_1)
#combined
slopes_2 <- full_join(slopes_p, slopes_t, by= c("state", "name",
                                                "lon", "lat"))
hist(slopes_2$slope_p_1)
hist(slopes_2$slope_t_1)
#To do the map I used ggplot2
grad <- na.omit(slopes_2)
map_1 <- map_data("state")
ggplot() +
  geom_polygon(data = map_1, aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
  geom_point(data = slopes_2, aes(x = lon, y = lat, color = grad$slope_p_1), size = 2) +
  scale_color_gradient(low = "yellow", high = "blue", na.value = "grey") +  
  labs(title = "Map of Precipitation Slopes", color = "Slope (Precip)") +
  theme_minimal() +
  coord_fixed(1.3)
ggplot() +
  geom_polygon(data = map_1, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "white") +
  geom_point(data = slopes_2, aes(x = lon, y = lat, color = slopes_2$slope_t_1), size = 2) +
  scale_color_gradient(low = "green", high = "orange2", na.value = "grey") + 
  labs(title = "Map of Temperature Slopes", color = "Slope (Temperature)") +
  theme_minimal() +
  coord_fixed(1.3)


