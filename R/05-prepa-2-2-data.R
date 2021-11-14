
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

## Corr LU code
plot_init2 <- plot_init %>%
  mutate(lc = if_else(lu_code == "DE", "DD", lu_code)) %>%
  rename(lc_name = lu_factor) %>%
  select(-lu_code)

## Randomly select plots
plot_list <- map(.x = lc_list, .f = function(x){
  
  plot_sub <- plot_init2 %>% filter(lc == x) %>% pull(plot_id)
  plot_n  <- nplot4_uneven %>% filter(lc == x) %>% pull(n)
  n <- ifelse(length(plot_sub) < plot_n, length(plot_sub), plot_n)
  
  set.seed(10)
  plot_pos <- sample(1:length(plot_sub), n, replace = F)
  
  plot_sub[plot_pos]
  
}) %>% unlist()

plot_list
length(plot_list)

## Subset plot and trees
plot <- plot_init2 %>%
  filter(plot_id %in% plot_list)

tree <- tree_init %>%
  filter(plot_id %in% plot_list)

##
## Checks ###################################################################
##


set.seed(36)

## make 10% tree measurement H error 
tree_me <- sample(x = 1:nrow(tree), size = round(nrow(tree) * 0.002)) 

tree2 <- tree %>%
  left_join(species_list, by = "sp_id") %>%
  left_join(plot, by = "plot_id") %>%
  left_join(wd_species, by = "sp_name") %>%
  left_join(wd_genus, by = "genus") %>%
  mutate(
    tree_num   = 1:nrow(.),
    h_chave    = exp(0.893 - envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    h_residual = exp(rnorm(n = dim(.)[1], mean = 0, sd = 0.243)),
    h_me       = if_else(tree_num %in% tree_me, 10, 1), ## Random measurement error h multiplied by 2
    h_est      = h_chave * h_residual,
    h          = if_else(tree_height_top < 2 & tree_dbh > 20, tree_height_top, h_est * h_me),
    h_ci       = 0.243 * h_chave * 1.96
  ) %>%
  mutate(
    tree_height_valid  = case_when(
      tree_height_top > h_chave + h_ci ~ "Above CI", 
      tree_height_top < h_chave - h_ci ~ "Below CI",
      TRUE ~ "Within CI"
    ),    
    tree_height_cor    = if_else(tree_height_valid == 1 | is.na(tree_height_top), h_chave, tree_height_top),
    tree_height_origin = if_else(tree_height_valid == 1 | is.na(tree_height_top), "model", "data"),
  )

tree2 %>%
  ggplot(aes(x = tree_dbh)) +
  geom_line(aes(y = h_chave, color = envir_stress, group = envir_stress)) +
  scale_color_viridis_c()

tree2 %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = h_est, color = envir_stress, group = envir_stress), size = 0.6) +
  scale_color_viridis_c() +
  facet_grid(tree_health ~ lc)

tree2 %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = h, color = envir_stress, group = envir_stress)) +
  geom_point(data = . %>% filter(h_me == 10), aes(y = h), shape = 21, size = 3, col = "red") +
  scale_color_viridis_c()

tree2 %>%
  filter(h_me != 10) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = h, color = envir_stress, group = envir_stress)) +
  #geom_point(data = . %>% filter(h_me == 10), aes(y = h), shape = 21, size = 3, col = "red") +
  scale_color_viridis_c()


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
  #facet_grid(tree_health~lc) +
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
  facet_wrap(~lc) +
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
  geom_point(aes(y = tree_height_cor), col = "darkred") +
  facet_grid(tree_height_valid~lc) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Tree total height (m)"
  )
gr_hd2

gr_hd3 <- tree_agb %>%
  filter(!(tree_id %in% tree_out$tree_id)) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(data = . %>% filter(tree_height_valid == 1), aes(y = tree_height_top), shape = 3) +
  geom_point(aes(y = tree_height_cor), color = "darkred", size = 0.6) +
  facet_wrap(~lc) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Tree total height (m)",
    color = "Tree Health"
  )
gr_hd3

gr_hd3 <- tree_agb %>%
  filter(!(tree_id %in% tree_out$tree_id)) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_cor, color = tree_height_origin), size = 0.6) +
  scale_color_manual(values = c("black", "darkred")) +
  geom_segment(data = . %>% filter(tree_height_valid == 1), aes(xend = tree_dbh, y = tree_height_top, yend= tree_height_cor), col = "darkred") +
  #geom_line(aes(y = tree_height_chave, color = envir_stress, group = envir_stress)) +
  facet_wrap(~lc) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Tree total height (m)",
    color = "Tree Health"
  )
gr_hd3

## Check model and ci for one plot
"md-zzb5" %in% plot$plot_id

tree_agb %>%
  filter(plot_id == "md-zzb5") %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, color = as.character(tree_height_valid))) +
  geom_line(aes(y = tree_height_chave), size = 1) +
  #geom_line(aes(y = tree_height_chave + tree_height_ci)) +
  #geom_line(aes(y = tree_height_chave - tree_height_ci)) +
  geom_ribbon(aes(ymin = tree_height_chave - tree_height_ci, ymax = tree_height_chave + tree_height_ci), fill="blue", alpha=0.2) +
  theme_bw() +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Tree total height (m)",
    color = "Height validation"
  )

plot_sub <- plot %>%
  filter(lc == "EV") %>%
  pull(plot_id) 

set.seed(10)
plot_rd <- sample(1:length(plot_sub), 10)

plot_sub <- plot_sub[plot_rd]

walk(plot_sub, function(x){
  
  gr <- tree_agb %>%
    filter(plot_id == x) %>%
    ggplot(aes(x = tree_dbh)) +
    geom_point(aes(y = tree_height_top, color = as.character(tree_height_valid))) +
    geom_line(aes(y = tree_height_chave), size = 1) +
    #geom_line(aes(y = tree_height_chave + tree_height_ci)) +
    #geom_line(aes(y = tree_height_chave - tree_height_ci)) +
    geom_ribbon(aes(ymin = tree_height_chave - tree_height_ci, ymax = tree_height_chave + tree_height_ci), fill="blue", alpha=0.2) +
    theme_bw() +
    labs(
      x = "Diameter at breast height (cm)",
      y = "Tree total height (m)",
      color = "Height validation"
    )
  print(gr)
  
})





