library("dplyr")
library("wordVectors")
library("proxy")

create_aspect_pairs <- function(modelo.data){
  similarities_matrix = proxy::simil(modelo.data, method="cosine", diag = T) %>% as.matrix()
  similarities_matrix[is.na(similarities_matrix)] = 1
  return(similarities_matrix)
}

get_similarity = function(item_1, item_2, sim_matrix){
  return(sim_matrix[item_1,item_2])
}

user_similarity_to_one_subaspect = function(subaspect, user, similarities){
  user_subaspect = expand.grid(user, subaspect) %>% rename(user = Var1, subaspect = Var2)
  user_subaspect$user = user_subaspect$user %>% as.character()
  user_subaspect$subaspect = user_subaspect$subaspect %>% as.character()
  
  user_pairs_weights = user_subaspect %>% rowwise() %>% mutate(sim = get_similarity(user, subaspect, similarities))
  user_item_weights = user_pairs_weights %>% group_by(user) %>% summarise(item_similarity = mean(sim))
  user_weight = user_item_weights %>% summarise(user_weight = mean(item_similarity)) %>% as.numeric()
  return(user_weight)
}

user_similarity_to_subaspects <- function(user, list_of_subaspects, similarities){
  user_subaspects_weights = sapply(list_of_subaspects, user_similarity_to_one_subaspect, user, similarities)
  return(user_subaspects_weights)
}

users_similarities_to_subaspects <- function(list_of_users, list_of_subaspects, pairs_similarity){
  user_subaspects_weights = lapply(list_of_users, user_similarity_to_subaspects, list_of_subaspects, pairs_similarity)
  return(user_subaspects_weights)
}
