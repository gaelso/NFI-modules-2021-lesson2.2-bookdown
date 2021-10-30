
## FAO NFI technical module 9: Practice
## Lesson 2: Simple sampling for carbon
## Part 2: Data analysis
## NFI-modules-2021-lesson2.2
## Gael Sola, FAO
## October 2021


##
## Assign plots and trees ###################################################
## 

table(plot_init$lu_code)
table(sf_plot4_uneven$lc)
nrow(sf_plot4_uneven)

lc_list <- c('WL', 'DD', 'MD', 'EV', 'MG')

nplot4_uneven %>%
  filter(lc %in% lc_list) %>%
  summarise(n = sum(n))

## Corr LU coe
plot_init <- plot_init %>%
  mutate(lu_code = if_else(lu_code == "DE", "DD", lu_code))

## Randomly select plots
plot_list <- map(.x = lc_list, .f = function(x){
  
  plot_sub <- plot_init %>% filter(lu_code == x) %>% pull(plot_id)
  plot_n  <- nplot4_uneven %>% filter(lc == x) %>% pull(n)
  n <- ifelse(length(plot_sub) < plot_n, length(plot_sub), plot_n)
  
  set.seed(10)
  plot_pos <- sample(1:length(plot_sub), n, replace = F)
  
  plot_sub[plot_pos]
  
}) %>% unlist()

plot_list
length(plot_list)

## Subset plot and trees
plot <- plot_init %>%
  filter(plot_id %in% plot_list)

tree <- tree_init %>%
  filter(plot_id %in% plot_list)

##
## Calculations #############################################################
##

## Assign species
tree2 <- tree %>%
  left_join(species_list, by = "sp_id") %>%
  left_join(plot, by = "plot_id") %>%
  left_join(wd_species, by = "sp_name") %>%
  left_join(wd_genus, by = "genus")

## Data check
gr_hd <- ggplot(tree2) +
  geom_point(aes(x=tree_dbh, y= tree_height_top, color = as.character(tree_health))) +
  scale_color_viridis_d() +
  facet_grid(tree_health~lu_code) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)", 
    y = "Tree total height (m)",
    color = "Tree Health"
    )
gr_hd

## Health

tree_out <- tree2 %>%
  filter(tree_health == 0, tree_dbh > 10, tree_height_top < 2)

gr_health <- tree2 %>%
  ggplot(aes(x=tree_dbh, y= tree_height_top)) +
  geom_point(aes(color = as.character(tree_health))) +
  geom_point(data = tree_out, shape = 21, size = 3, col = "red") +
  scale_color_viridis_d() +
  #facet_grid(tree_health~lu_code) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)", 
    y = "Tree total height (m)",
    color = "Tree Health"
  ) 
gr_health


## H_corr  
tree_agb <- tree2 %>%
  mutate(
    tree_height_chave  = exp(0.893 - envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    tree_height_ci     = 0.243 * tree_height_chave * 1.96,
    tree_height_valid  = case_when(
       tree_height_top > tree_height_chave + tree_height_ci ~ 1, 
       tree_height_top < tree_height_chave - tree_height_ci ~ -1,
       TRUE ~ 0
       ),    
    tree_height_cor    = if_else(tree_height_valid == 1 | is.na(tree_height_top), tree_height_chave, tree_height_top),
    tree_height_origin = if_else(tree_height_valid == 1 | is.na(tree_height_top), "model", "data"),
  )


gr_hd2 <- tree_agb %>%
  filter(!(tree_id %in% tree_out$tree_id)) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top)) +
  geom_line(aes(y = tree_height_chave, color = envir_stress, group = envir_stress)) +
  scale_color_viridis_c() +
  facet_wrap(~lu_code) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Tree total height (m)",
    color = "Environmental stress (E)"
  )
gr_hd2

gr_hd2 <- tree_agb %>%
  filter(!(tree_id %in% tree_out$tree_id)) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, color = as.character(tree_height_valid))) +
  #scale_color_viridis_d() +
  #geom_point(aes(y = tree_height_cor), col = "darkred") +
  facet_grid(tree_height_valid~lu_code) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Tree total height (m)",
    color = "Tree Health"
  )
gr_hd2

gr_hd3 <- tree_agb %>%
  filter(!(tree_id %in% tree_out$tree_id)) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_cor, color = tree_height_origin)) +
  scale_color_manual(values = c("black", "darkred")) +
  geom_segment(data = . %>% filter(tree_height_valid == 0), aes(xend = tree_dbh, y = tree_height_top, yend= tree_height_cor), col = "darkred") +
  #geom_line(aes(y = tree_height_chave, color = envir_stress, group = envir_stress)) +
  facet_wrap(~lu_code) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Tree total height (m)",
    color = "Tree Health"
  )
gr_hd3

