library(ggplot2)

countdata = read.csv(url("https://raw.githubusercontent.com/jgscott/STA380/master/data/congress109.csv"), header=TRUE, row.names=1)
memberdata = read.csv(url("https://raw.githubusercontent.com/jgscott/STA380/master/data/congress109members.csv"), header=TRUE, row.names=1)

# First normalize phrase counts to phrase frequencies.
# (often a sensible first step for count data, before z-scoring)
Z = countdata/rowSums(countdata)

# PCA
pc2 = prcomp(Z, scale=TRUE, rank=2) #rank = 2 ??? #there will be PC529
loadings = pc2$rotation
scores = pc2$x

# Question 1: where do the observations land in PC space?
# a biplot shows the first two PCs
qplot(scores[,1], scores[,2], color=memberdata$party, xlab='Component 1', ylab='Component 2')

# Confusingly, the default color mapping has Democrats as red and republicans as blue.  This might be confusing, so let's fix that:
qplot(scores[,1], scores[,2], color=memberdata$party, xlab='Component 1', ylab='Component 2') + scale_color_manual(values=c("blue", "grey", "red"))

# Interpretation: the first PC axis primarily gas Republicans as positive numbers and Democrats as negative numbers
# PC1 left: Demoncrate; right: Republicant
# PC2 ambiguous, hard to interpretate

# Question 2: how are the individual PCs loaded on the original variables?
# Which X serve more important??
# The top words associated with each component
o1 = order(loadings[,1], decreasing=TRUE) # sort Xs in PC1 from highest positive to hightest negative
colnames(Z)[head(o1,25)] #most positive, might be the topics more discuss by Republicants
colnames(Z)[tail(o1,25)] #the most Demoncrative phrases

o2 = order(loadings[,2], decreasing=TRUE)# sort Xs in PC2 from highest positive to hightest negative
colnames(Z)[head(o2,25)]
colnames(Z)[tail(o2,25)]
