## FAO NFI technical module 9: Practice
## Lesson 2: Simple sampling for carbon
## Part 2: Data analysis
## NFI-modules-2021-lesson2.2
## Gael Sola, FAO
## October 2021

tree2 <- tree %>% 
  left_join(plot, by = "plot_id") %>%
  left_join(species_list, by = "sp_id") %>%
  left_join(wd_species, by = "sp_name") %>%
  left_join(wd_genus, by = "genus")


tree2 %>%
  ggplot(aes(x = tree_dbh, y = tree_height_top)) +
  geom_point()


out_top <- tree2 %>%
  filter(tree_height_top > 60)
out_bot <- tree2 %>%
  filter(tree_height_top < 2)

tree2 %>%
  ggplot(aes(x = tree_dbh, y = tree_height_top)) +
  geom_point() +
  geom_point(data = out_top, shape = 21, size = 4, col = "red") +
  geom_point(data = out_bot, shape = 21, size = 4, col = "blue")

##
## Tree_height ##############################################################
##

## --- Testing Feldpausch 2013: Tree height integrated into pan-tropical forest biomass estimates.
tree_h <- tree2 %>%
  mutate(
    h_check = 50.874 * (1 - exp(-0.0420 * tree_dbh^0.784)),
    h_me    = 5.479 * 1.96
    )

tree_h %>%
  filter(tree_height_top > 2) %>%
  mutate(tree_height_top = if_else(tree_id %in% out_top$tree_id, tree_height_top/10, tree_height_top)) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top)) +
  geom_line(aes(y = h_check), col = "red") +
  geom_ribbon(aes(ymin = h_check - h_me, ymax = h_check + h_me), fill = "red", alpha = 0.2)
## Not great
## ---


## --- Testing homemade model
tree_cor <- tree2 %>%
  filter(tree_height_top > 2) %>%
  mutate(tree_height_top = if_else(tree_id %in% out_top$tree_id, tree_height_top/10, tree_height_top))

tree_cor %>%
  ggplot(aes(x = tree_dbh, y = tree_height_top)) +
  geom_point()

## <-- Model
hd <- lm(log(tree_height_top) ~ log(tree_dbh), data = tree_cor)

summary(hd)
coef(hd)
plot(hd)

hd_cf <- exp(summary(hd)$sigma^2 / 2) ## Correction Factor
## -->


tree_cor2 <- tree_cor %>%
  mutate(
    h_check = hd_cf * exp(coef(hd)[1] + coef(hd)[2] * log(tree_dbh)),
    h_me    = h_check * 1.96 * summary(hd)$sigma 
  )

tree_cor2 %>%
#  filter(tree_dbh < 20) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top)) +
  geom_line(aes(y = h_check), col = "red") +
  geom_ribbon(aes(ymin = h_check - h_me, ymax = h_check + h_me), fill = "red", alpha = 0.2) +
  facet_wrap(~lc)



## <-- Model per land cover
hd_lc <- map_dfr(unique(tree_cor$lc), function(x){
  
  data <- tree_cor %>% 
    filter(lc == x) %>%
    filter(round((tree_no - 1) / 5) == (tree_no - 1 ) / 5)
  
  hd <- lm(log(tree_height_top) ~ log(tree_dbh), data = data)
  
  tibble(lc = x, cf = exp(summary(hd)$sigma^2 / 2), a = coef(hd)[1], b = coef(hd)[2], me = 1.96 * summary(hd)$sigma)
  
})

hd_lc

tree_cor3 <- tree_cor %>% 
  left_join(hd_lc, by = "lc") %>%
  mutate(
    h_est = cf * exp(a) * tree_dbh^b,
    h_me  = h_est * me
    )

tree_cor3 %>%
  filter(round((tree_no - 1) / 5) == (tree_no - 1) / 5) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, fill = lc), shape = 21, alpha = 0.2) +
  #geom_point(data = filter(tree_cor3, round((tree_no - 1) / 5) == (tree_no - 1) / 5), aes(y = tree_height_top), col = "darkred") +
  geom_line(aes(y = h_est, color = lc)) +
  geom_ribbon(aes(ymin = h_est - h_me, ymax = h_est + h_me, fill = lc), alpha = 0.1) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  facet_wrap(~lc) +
  theme_bw()



## 
## Tree wood density ########################################################
##

tree_wd <- tree_cor3 %>%
  mutate(
    tree_wd = case_when(
      !is.na(wd_avg)  ~ wd_avg,
      !is.na(wd_avg2) ~ wd_avg2,
      TRUE            ~ 0.53
      ),
    tree_wd_level = case_when(
      !is.na(wd_avg)  ~ "species",
      !is.na(wd_avg2) ~ "genus",
      TRUE            ~ "default"  
      )
    )

table(tree_wd$tree_wd_level, useNA = "always")


## 
## Tree aboveground biomass #################################################
##

tree_agb <- tree_wd %>%
  mutate(
    tree_ba   = (tree_dbh / 200)^2 * pi,
    tree_surb = tree_wd * (tree_dbh/200)^2 * tree_height_top,
    tree_agb  = 0.0673 * (tree_wd * tree_dbh^2 * tree_height_top)^0.976 / 10^3,
    subplot_radius = if_else(tree_dbh < 20, 5, 20),
    scale_factor = 10000 / (subplot_radius^2 * pi)
    )

tree_agb %>%
  ggplot(aes(x = tree_dbh, y = tree_agb)) +
  geom_point(aes(fill = lc), shape = 21)

tree_agb %>%
  ggplot(aes(x = tree_ba, y = tree_agb)) +
  geom_point(aes(fill = lc), shape = 21)

tree_agb %>%
  ggplot(aes(x = tree_surb)) +
  geom_line(aes(y = tree_agb)) +
  geom_ribbon(aes(ymin = tree_agb*(1 - 1.96*0.357), ymax = tree_agb*(1 + 1.96*0.357)), fill = "red", alpha = 0.2)



## 
## Plot AGB #################################################################
##

plot_agb_prepa <- tree_agb %>%
  group_by(plot_id) %>%
  summarise(
    plot_density = sum(scale_factor),
    plot_ba      = sum(tree_ba * scale_factor),
    plot_agb     = sum(tree_agb * scale_factor)
   ) 

plot_agb <- plot_agb_prepa %>%
  left_join(plot, by = "plot_id")

out_agb <- plot_agb %>%
  filter(plot_ba > 40 | plot_agb > 500)

plot_agb %>%
  ggplot(aes(x = plot_ba, y = plot_agb)) +
  geom_point(aes(fill = lc), shape = 21, size = 2, alpha = 0.7) +
  ggrepel::geom_label_repel(data = out_agb, aes(label = plot_id), min.segment.length = 0) +
  scale_fill_viridis_d()


tree_out <- tree_agb %>%
  filter(plot_id %in% out_agb$plot_id)


sf_plot_agb <- sf_plot %>% 
  right_join(plot_agb_prepa, by = "plot_id")

rgb <- col2rgb(pal)
g <- pal2[1,] * 0.299 + pal2[2,] * 0.587 + pal2[3,] * 0.114
pal2 <- rgb(g, g, g, maxColorValue=255)


gr_plot_agb <- ggplot() +
  geom_sf(data = sf_lc, aes(fill = lc), color = NA, alpha = 0.8) +
  geom_sf(data = sf_plot_agb, aes(color = plot_agb), size = 2) +
  #geom_sf(data = sf_grid4, fill = NA, col = "red", size = 0.1) +
  geom_sf(data = sf_admin, fill= NA) +
  scale_fill_manual(values = pal2) +
  scale_color_viridis_c() +
  labs(fill = "", color = "") +
  theme_bw()
gr_plot_agb

gr_plot4  



##
## Forest AGB ###############################################################
##

forest_agb_tot <- plot_agb %>%
  summarise(
    n_plot = n(),
    mean_agb = mean(plot_agb),
    sd_agb = sd(plot_agb)
  ) %>%
  mutate(
    me = 1.96 * sd_agb / sqrt(n_plot),
    lc = "Total")
forest_agb_tot

forest_agb_lc <- plot_agb %>%
  group_by(lc) %>%
  summarise(
    n_plot = n(),
    mean_agb = mean(plot_agb),
    sd_agb = sd(plot_agb)
  ) %>%
  mutate(
    me = 1.96 * sd_agb / sqrt(n_plot),
    lc = as.character(lc)
    )
forest_agb_lc

forest_agb <- bind_rows(forest_agb_lc, forest_agb_tot) %>%
  mutate(lc = factor(lc, levels = c("WL", "DD", "MD", "EV", "MG", "Total")))

forest_agb %>%
  ggplot(aes(x = lc, y = mean_agb)) +
  geom_col(aes(fill = lc), col = "black") +
  scale_fill_viridis_d(direction = -1) +
  geom_errorbar(aes(ymin = mean_agb - me, ymax = mean_agb + me), width = 0.3) +
  geom_label(aes(label = n_plot, y = 8), fill = "white", alpha = 0.3, position = position_nudge(-0.25)) + 
  theme_bw() +
  theme(legend.position = "none")

