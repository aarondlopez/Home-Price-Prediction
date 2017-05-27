#load data set from download saved to plain txt file
movies <- read.table("movielens.txt", header=FALSE, sep="|", quote="\"")
str(movies)
#rename colums
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMBD", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "Scifi", "Thriller", "War", "Western")
str(movies)
#remove columns we don't want
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMBD = NULL
movies = unique(movies)
str(movies)
# find the distances between each point for our movie genres
distances = dist(movies[2:20], method = "euclidean")
# use those distances to cluster our movies 
clusterMovies = hclust(distances, method = "ward.D")
# plot a Dendrogram to find where our clusters form
plot(clusterMovies)
# create 10 cluster groups
clustergroups = cutree(clusterMovies, k = 10)
# which clusters do Action movies fall in?
tapply(movies$Action, clustergroups, mean)
# which clusters do Romance movies fall in?
tapply(movies$Romance, clustergroups, mean)
# what generes does Men in Black fall under? Action, Adventure, Comedy, Scifi
subset(movies, Title == "Men in Black (1997)")
# This is cluster 2 based on our groupings
cluster2 = subset(movies, clustergroups == 2)
# What other movies would you recommend for someone who likes Men in Black? These movies all fall in the same cluster as Men in Black
cluster2$Title[1:10]
