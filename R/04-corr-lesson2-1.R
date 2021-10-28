
## FAO NFI technical module 9: Practice
## Lesson 2: Simple sampling for carbon
## Part 2: Data analysis
## NFI-modules-2021-lesson2.3
## Gael Sola, FAO
## October 2021

##
## Make cover image #########################################################
##

offset <- st_bbox(sf_lc)[c("xmin", "ymin")] + c(-1000, -1000)

sf_grid4 <- st_make_grid(sf_lc, cellsize = c(4000, 4000), what = "polygons", offset = offset) %>%
  st_intersection(sf_admin)

sf_points4 <- st_make_grid(sf_lc, cellsize = c(4000, 4000), what = "centers", offset = offset) %>%
  st_intersection(sf_admin) %>%
  st_as_sf()

set.seed(10)
sf_points4_uneven <- st_sample(x = sf_grid4, size = rep(1, nrow(sf_points4))) %>%
  st_as_sf()

sf_plot4_uneven <- sf_points4_uneven %>%
  st_join(sf_lc) %>%
  mutate(lc = fct_reorder(lc, lc_id)) %>%
  filter(!is.na(lc))

gr_un4 <- ggplot() +
  geom_sf(data = sf_lc, aes(fill = lc), color = NA) +
  geom_sf(data = sf_grid4, fill = NA, col = "red") +
  geom_sf(data = sf_plot4_uneven, aes(fill = lc), shape = 21) +
  geom_sf(data = sf_admin, fill = NA) +
  scale_fill_manual(values = pal) +
  labs(fill = "", color = "") +
  theme_void()
gr_un4

nplot4_uneven <- sf_plot4_uneven %>%
  as_tibble() %>%
  group_by(lc) %>%
  summarise(n = n())
nplot4_uneven

