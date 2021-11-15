
## FAO NFI technical module 9: Practice
## Lesson 2: Simple sampling for carbon
## Part 2: Data analysis
## NFI-modules-2021-lesson2.2
## Gael Sola, FAO
## October 2021


##
## Assign plots #############################################################
## 

table(plot_init$lu_code)
table(sf_plot4$lc)
nrow(sf_plot4)

lc_list <- c('WL', 'DD', 'MD', 'EV', 'MG')

nplot4 %>%
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
  plot_n  <- nplot4 %>% filter(lc == x) %>% pull(n)
  n <- ifelse(length(plot_sub) < plot_n, length(plot_sub), plot_n)
  
  set.seed(10)
  plot_pos <- sample(1:length(plot_sub), n, replace = F)
  
  plot_sub[plot_pos]
  
}) %>% unlist()

plot_list
length(plot_list)

## Create plot IDs
sf_group <- sf_plot4 %>%
  mutate(
    plot_x = st_coordinates(.)[,1],
    plot_y = st_coordinates(.)[,2],
    crs = 32727
  ) %>%
  st_transform(crs = 4326) %>%
  mutate(
    num = 1:nrow(.),
    plot_long = st_coordinates(.)[,1],
    plot_lat = st_coordinates(.)[,2],
    group  = case_when(
      plot_long > -20  ~ 3,
      plot_lat > -0.3 ~ 2,
      TRUE          ~ 1
    ),
  ) %>%
  group_by(group) %>%
  mutate(
    num  = row_number(),
    num2 =case_when(
      trunc(num/100) > 0 ~ as.character(num),
      trunc(num/10)  > 0 ~ paste0("0", num),
      TRUE               ~ paste0("00", num)
      )
    ) %>%
  ungroup() %>%
  mutate(plot_id = paste0("g", group, num2)) %>%
  group_by(lc) %>%
  mutate(join_id = paste0(lc, "_", row_number())) %>%
  ungroup() %>%
  as_tibble() %>%
  select(join_id, plot_id, plot_x, plot_y, crs, plot_long, plot_lat)
sf_group


## Subset plot and trees
plot_prepa <- plot_init2 %>%
  filter(plot_id %in% plot_list) %>%
  rename(plot_id_old = plot_id) %>%
  group_by(lc) %>%
  mutate(join_id = paste0(lc, "_", row_number())) %>%
  left_join(as_tibble(sf_group), by = "join_id")



##
## Revise tree height #######################################################
##

tree_prepa <- tree_init %>%
  filter(plot_id %in% plot_list) 

set.seed(100)

## make 10% tree measurement H error 
tree_me <- sample(x = 1:nrow(tree_prepa), size = round(nrow(tree_prepa) * 0.002)) ## select for error 2/1000 trees

tree_prepa <- tree_prepa %>%
  rename(plot_id_old = plot_id) %>%
  left_join(species_list, by = "sp_id") %>%
  left_join(plot_prepa, by = "plot_id_old") %>%
  left_join(wd_species, by = "sp_name") %>%
  left_join(wd_genus, by = "genus") %>%
  mutate(
    tree_num   = 1:nrow(.),
    h_chave    = exp(0.893 - envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    h_residual = exp(rnorm(n = nrow(.), mean = 0, sd = 0.243)),
    h_me       = if_else(tree_num %in% tree_me, 1, 0), ## Random measurement error for 1 no error for 0
    h_est      = h_chave * h_residual,
    h          = case_when(
      tree_height_top > 60 ~ h_est,
      tree_height_top < 2  ~ tree_height_top,
      h_me == 1            ~ h_est * 10, ## Multiply errors by 10 to simulate a misplaced comma 
      TRUE                 ~ (h_est + tree_height_top) / 2
      ),
    h = round(h, 1),
    h_ci       = 0.243 * h_chave * 1.96
  )


## Checks
summary(tree_prepa$envir_stress)

## Graphs

tree_prepa %>%
  ggplot(aes(x = tree_dbh)) +
  geom_line(aes(y = h_chave, color = envir_stress, group = envir_stress)) +
  scale_color_viridis_c()

tree_prepa %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = h_est, color = envir_stress, group = envir_stress), size = 0.6) +
  scale_color_viridis_c()

tree_prepa %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_height_top, color = envir_stress, group = envir_stress), size = 0.6) +
  scale_color_viridis_c()

tree_prepa %>%
  filter(h_me == 0) %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = h, color = envir_stress, group = envir_stress), size = 0.6) +
  scale_color_viridis_c()

tree_prepa %>%
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = (h_est + tree_height_top)/2, color = envir_stress, group = envir_stress), size = 0.6) +
  scale_color_viridis_c()


##
## Assign tree and plot #####################################################
##

plot <- plot_prepa %>% 
  select(plot_id, everything(), -plot_id_old, -join_id, -envir_stress)
  
tree <- tree_prepa %>%
  arrange(lc, plot_id) %>%
  mutate(tree_id = paste0(plot_id, "-", tree_no)) %>%
  select(plot_id, tree_id, tree_no, tree_dbh, tree_height_top = h, tree_health, sp_id)

rm(lc_list, plot_list, tree_me, sf_group)


tree %>%
  group_by(tree_id) %>%
  summarize(count = n()) %>%
  filter(count > 1)

## Checks

length(unique(plot_prepa$plot_id)) == nrow(plot_prepa)

length(unique(tree_prepa$tree_id)) == nrow(tree_prepa)

length(unique(tree$tree_id)) == nrow(tree)

