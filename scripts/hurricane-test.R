library(stormwindmodel)

data("floyd_tracks")
full_track <- create_full_track(hurr_track = floyd_tracks, tint = 0.25)
full_track %>% slice(1:3)



library(sf)
library(maps)
library(ggplot2)

floyd_states <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>% 
  dplyr::filter(ID %in% c("north carolina", "south carolina", "maryland",
                          "georgia", "florida", "virginia", "delaware", 
                          "pennsylvania", "west virginia", "new jersey",
                          "new york"))

floyd_test = 


floyd_15_min <- create_full_track(floyd_tracks)
floyd_2_hrs <- create_full_track(floyd_tracks, tint = 2)

floyd_15_min <- floyd_15_min %>% 
  mutate(tclon = -tclon) %>% 
  st_as_sf(coords = c("tclon", "tclat")) %>% 
  st_set_crs(4326) %>%
  filter(row_number() <= 5)


floyd_2_hrs <- floyd_2_hrs %>% 
  mutate(tclon = -tclon) %>% 
  st_as_sf(coords = c("tclon", "tclat")) %>% 
  st_set_crs(4326)

a <- ggplot() + 
  geom_sf(data = floyd_states, 
          fill = "aliceblue") + 
  xlim(c(-90, -70)) + 
  ylim(c(24, 46))
b <- a +
  geom_sf(data = floyd_15_min,
          size = 0.5, color = "red") + 
  ggtitle("Interpolated to 15 minutes")
c <- a + 
  geom_sf(data = floyd_2_hrs,
          size = 0.5, color = "red") + 
  ggtitle("Interpolated to 2 hours") 

gridExtra::grid.arrange(b, c, ncol = 2)

floyd_15_min = create_full_track(floyd_tracks, tint = 2) %>% 
  mutate(
    e1 = rnorm(mean = 0, sd = 0.5, n = 142),
    e2 = rnorm(mean = 0, sd = 0.5, n = 142),
    #tclat = tclat + e1,
    tclon = -(tclon + e2)
    ) %>% 
  st_as_sf(coords = c("tclon", "tclat")) %>%
  #st_cast("LINESTRING") %>%
  st_set_crs(4326) %>%
  plot(lty = 3)

floyd_real = create_full_track(floyd_tracks) %>% 
  mutate(
    tclon = -tclon
  ) %>% 
  st_as_sf(coords = c("tclon", "tclat")) %>% 
  st_set_crs(4326)


ggplot() + 
  geom_sf(data = floyd_states, fill = "aliceblue") + 
  xlim(c(-90, -70)) + 
  ylim(c(24, 46))  +
  geom_sf(data = floyd_15_min, size = 0.25, color = "red") +
  #geom_sf(data = floyd_real, size = 0.25, color = "blue") +
  ggtitle("Interpolated to 15 minutes")



floyd_15_min = create_full_track(floyd_tracks, tint = 2) %>% 
  mutate(
    e1 = rnorm(mean = 0, sd = 0.5, n = 142),
    e2 = rnorm(mean = 0, sd = 0.5, n = 142),
    lat = tclat,
    lon = -(tclon + e2)
  ) %>% 
  group_by(lat, lon) %>%
  summarise(plat = mean(lat), plon = mean(lon))


ggplot()  + 
  geom_sf(data = floyd_states, fill = "aliceblue") + 
  xlim(c(-90, -70)) + 
  ylim(c(24, 46)) +
  geom_sf(data = floyd_real, size = 0.1, color = "blue") +
  geom_path(data = floyd_15_min, aes(x = plon, y = plat)) +
  geom_smooth(data = floyd_15_min, aes(x = lat, y = lon), color = "red")
