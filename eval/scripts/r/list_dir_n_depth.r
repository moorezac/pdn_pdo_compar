list_dir_n_depth <- function(path_use, depth_keep = 1){
  
  path_list = list()
  level = 1
  path_list[[level]] = path_use # initiate first level 
  q = path_list[[level]]
  
  # loop it till no child level left, or reached depth_keep
  while(length(q) > 0 & level < depth_keep){
    level = level + 1 # advance one level to add all the child nodes
    path_list[[level]] = vector() # initiate child list
    
    for(i in seq(length(q), 1, -1)){ 
      # when there are more than one item, collect all the child for next level 
      all_childs_this_dir = list.dirs(q[[i]], recursive = FALSE) 
      # get all childs
      path_list[[level]] = c(path_list[[level]], all_childs_this_dir) 
      # add to current level list
    }
    
    # get all items from next level
    q = path_list[[level]]
    
  }
  # return the level to print
  if(depth_keep > length(path_list)){
    return(NULL)
    } else {
      return(path_list[[depth_keep]])
    }
}
