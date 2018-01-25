# converte rota para caminho ----------------------------------------------

tour_to_path <- function(tour, distance_vec, n) {
   max_dist <- which.max(distance_vec)
   if (max_dist == n) {
      cut_point <- as.integer(tour)[1]
   } else {
      cut_point <- as.integer(tour)[max_dist + 1]
   }
   path <- 
      tour %>% 
      cut_tour(cut_point[1], exclude_cut = F) %>% 
      unname()
   return(path)
}