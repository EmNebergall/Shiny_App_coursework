ordinate <- function(ord_method, dist_mat) {
  if (ord_method == "PCoA") {
    ape::pcoa(dist_mat)
  } else {
    if (ord_method == "NMDS") {
      metaMDS(
        dist_mat,
        distance = NULL,
        autotransform = FALSE,
        k = 2
      )
      
    }
  }
}

measure_distance <- function(dis_method, selected_data) {
    if (dis_method == "Euclidean") {
      dist_mat <- vegdist(selected_data, method = "euclidean")
    } else {
      if (dis_method == "Bray Curtis") {
        dist_mat <- vegdist(selected_data)
      } else {
        if (dis_method == "Jaccard") {
          dist_mat <- vegdist(selected_data, method = "jaccard")
        }
      }
    }
    return(dist_mat = dist_mat)
  }
  
data(dune, dune.env)


my_dist <- measure_distance("Euclidean", dune)

#my_ord <- ordinate("PCoA", my_dist) 


the_ord <- function(selected_ord_method, dist_mat) {
  my_ord <- ordinate(selected_ord_method, dist_mat)
  
  vectors <- my_ord$vectors
  values <- my_ord$values
  
  return(list(vectors = vectors, values = values))
}
  

thing <- the_ord("PCoA", my_dist)


  # perform the ordination, results in the_ord object
vectors <- thing$vectors
values <- thing$values

vectors
values

# dataframe to feed to ggplot
plot_df <- data.frame(
      axis_1 = thing$vectors[,1],
      axis_2 = thing$vectors[,2],
      env_color = dune.env[,2],
      env_shape = dune.env[,3])




ggplot(data = plot_df,
       mapping = aes(
         x = axis_1,
         y = axis_2,
         color = env_color,
         shape = env_shape
       )) +
  geom_point(size = 4) +
  xlab("Axis 1") +
  ylab("Axis 2")

class(plot_df)
class(plot_df$axis_2)  
