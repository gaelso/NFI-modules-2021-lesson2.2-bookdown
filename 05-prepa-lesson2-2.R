
## FAO NFI technical module 9: Practice
## Lesson 2: Simple sampling for carbon
## Part 2: Data analysis
## NFI-modules-2021-lesson2.2
## Gael Sola, FAO
## October 2021


##
## Assign plots #############################################################
## 

table(plot_init$lu_factor)
table(plot_init$lu_code)

table(sf_plot4_uneven$lc)

lc_list <- c('WL', 'DD', 'MD', 'EV', 'MG')

plot_list <- map(.x = lc_list, .f = function(x){
  
  plot_sub <- plot %>% filter(lu_code == x) %>% pull(plot_id)
  plot_n  <- nplot4_uneven %>% filter(lc == x) %>% pull(n)
  n <- ifelse(length(plot_sub) < plot_n, length(plot_sub), plot_n)
  
  set.seed(10)
  plot_pos <- sample(1:length(plot_sub), n, replace = F)
  
  plot_sub[plot_pos]
  
}) %>% unlist()

plot_list




