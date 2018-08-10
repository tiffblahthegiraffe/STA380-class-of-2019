
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# read file
social <- read.csv("C:/Users/Joseph/Desktop/jgscott git/data/social_marketing.csv", row.names = 1)

#drop columns spam, adult, and uncategorized from file
social = subset(social, select = -c(spam,adult,uncategorized))

#based on excel correlation analysis, group highly correlated variables together and create new categories:
#influencer, businessman, artists, familyguy, dude, fitness
social["influencer"] = social$chatter+social$photo_sharing+social$shopping+social$current_events+social$dating + social$cooking+social$beauty+social$fashion
social["businessman"] = social$travel + social$politics +social$computers +social$news + social$automotive +social$business
social['artists'] = social$tv_film +social$art +social$music +social$crafts +social$small_business
social['familyguy'] = social$sports_fandom + social$religion +social$parenting +social$school +social$food +social$family +social$home_and_garden
social['dude'] = social$online_gaming +social$college_uni +social$sports_playing
social['fit'] =social$outdoors+social$health_nutrition+social$personal_fitness+social$eco

#select new categories from file
social_new = social[, c(34:39)]

#make sure selected right coulmns (can drop this line)
names(social_new)

#scale social_new
social_scaled <- scale(social_new, center=TRUE, scale=TRUE)

#going forward, we hope to use clustering to technique
#to determine if our correlation-based grouping method
#gives us a interpretable and valid segmentation result




#select K
set.seed(123)
fviz_nbclust(social_scaled, kmeans, method = "wss")
#from elbow method, it is concluded that k=7 gives us best clustering result
fviz_nbclust(social_scaled, kmeans, method = "silhouette")
#again, K=7 returns best clusting result

#To triple check, we calculate CH
for (i in 2:10){
  final <- kmeans(social_scaled, centers = i , nstart = 25)
  B = final$betweenss
  final$withinss
  W = final$tot.withinss
  B/W
  n = nrow(social_scaled)
  k=i
  CH = (B/(k-1))/(W/(n-k))
  cat("k=", k, ", CH:", CH, "\n")

  
}

#when K=7, CH reches its max of 2006.149

#use K=7 to run kmeans
final <- kmeans(social_scaled, centers =7 , nstart = 25)

#display centers of seven clusters to see how they are allocated
print(final$centers)
###from the result, we can see that our hypothetical grouping method works well!
#each of the seven groups represents a distinct demographics
#no center in cluster one stands out, meaning that this group includes
#users that does not demostrate a particular interest on social media posts

#cluster two centers heavily on the businessman group, which includes people who like
#talking about topics like travel, travel, politics, computers, news, automotive, business

#cluster three centers on "dude," a proxy for people who enjoy topics like online_gaming, college_uni, sports_playing

#cluster four represents "artists." Variables include: tv_film, art, music, crafts, small_business

#cluster five centers on "familyguy." This category captures people who are "family oriented" and
#enojoy taking about topics like sports_fandom, religion, parenting, school, food, family, and home_and_garden

#cluster six captures "influencers," which can be seen as the millennials that like sharing lifestyle related topics on social media
#topics include:photo_sharing, shopping, current_events, dating, cooking, beauty, fashion

#cluster seven is "fit." These people are the outdoor enthuiast caring about health_nutrition, outdoors, personal_fitness, and eco



#lets see how many people belong in each group with this line of code:
final$size
#cluster1 is the biggest, which makes sense given the noisiness of the data and our narrowed-down groups
#cluster2 has 623 people, these are the "businessman" that care a lot about corporate, political, and electronics topics 
#cluster3 represents 410 "dudes," computer gamers and sports players in college
#cluster4 has 456 people. These are the artsy people that enjoy design, art, sensational experiences
#cluster5 summarizes 628 familyguy, people that value family, religion, and sports
#cluster6 gives us 1153 influencers. These are young millennials that love sharing about lifestyles on social media
#cluster7 has 834 "fit" people. These are the outdoor enthusiasts

#kmeans plot to show 7 clusters 
fviz_cluster(final, data = social_scaled, ellipse.type = "norm", stand = TRUE, geom = "point")


#from kmeans clustering, we can conclude that our hypothecial grouping method works very well 
#in identifying users with different interests on social media, or "socialgraphics"
#This output can help NutrientH20 better target its audience and focus their social media marketing
#efforts on a more defined and targeted group of people

