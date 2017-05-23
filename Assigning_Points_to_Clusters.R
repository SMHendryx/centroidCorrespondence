# Matching_Points_And_Clusters
# Authored by Sean Hendryx while working at the University of Arizona

#load packages:
library(data.table)

#define Euclidean distance function for working with two points/vectors in n-dim space:
euc.dist <- function(x1, x2){
  sqrt(sum((x1 - x2) ^ 2))
} 


assignPointsToClusters <- funtion(points, clusters, x_col_name = 'X', y_col_name = 'Y', cluster_id_col_name = 'Label', compute_centroids = TRUE){
  # this algorithm assigns point values to clusters.  For example, 
  # if we have a matrix of point coordinates, each of which should represent
  # a cluster in some way.
  # :Param points: data.table object with columns 'X' and 'Y'
  # :Param clusters: data.table object with columns 'X', 'Y', and 'Label'
  #check if column already exists:
  if(any(names(points) == "cluster_ID("){
    stop("cluster_ID already exists in points.  points should not include cluster ids prior to running assignPointsToClusters function.")
  }
  #if doesn't exist, add:
  points[,cluster_ID := integer()]
  clusterLabels = unique(clusters[,cluster_id_col_name])
  for(i in seq(nrow(points))){
    position = points[i, c(x_col_name, y_col_name)]
    minDist = Inf
    for(cluster in clusterLabels){
      if(compute_centroids){
        cluster_centroid = mean(clusters[cluster_id_col_name == cluster, c(x_col_name, y_col_name)])
      }else{
        cluster_centroid = clusters[cluster_id_col_name == cluster, c(x_col_name, y_col_name)]
      }
      distance = dist(position, cluster_centroid)
      if(distance < minDist){
        closestCluster = cluster
        minDist = distance
      }
    #assign closest cluster to point:
    # because we only loop through each point once, but loop through clusters for each point, assigning in this fashion effectively
    # assigns the points to the clusters because one cluster can be assigned multiple points while only 
    points[,cluster_ID := closestCluster]
    }
  return points
  }
}
