
setwd("C:\\Users\\kdoyl\\OneDrive\\Documents\\IST 707")

NovelsCorpus <- Corpus(DirSource("fedPapers"))
(getTransformations())
(ndocs<-length(NovelsCorpus))

(summary(NovelsCorpus))


(minTermFreq <- ndocs * 0.0001)
(maxTermFreq <- ndocs * 1)


Novels_dtm <- DocumentTermMatrix(NovelsCorpus,
                                 control = list(
                                   stopwords = TRUE, 
                                   wordLengths=c(3, 15),
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   tolower=T,
                                   stemming = T,
                                   remove_separators = T,
                                   bounds = list(global = c(minTermFreq, maxTermFreq))
                                 ))
inspect(Novels_dtm)

DTM_mat <- as.matrix(Novels_dtm)


(WordFreq <- colSums(as.matrix(Novels_dtm)))

(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])




(Row_Sum_Per_doc <- rowSums((as.matrix(Novels_dtm))))

Novels_M <- as.matrix(Novels_dtm)
Novels_M_N1 <- apply(Novels_M, 1, function(i) round(i/sum(i),3))
Novels_Matrix_Norm <- t(Novels_M_N1)

(Novels_M[c(1:6),c(1000:1005)])
(Novels_Matrix_Norm[c(1:6),c(1000:1005)])

Novels_dtm_matrix = as.matrix(Novels_dtm)
str(Novels_dtm_matrix)
(Novels_dtm_matrix[c(1:3),c(2:4)])

Novels_DF <- as.data.frame(as.matrix(Novels_dtm))
str(Novels_DF)


wordcloud(colnames(Novels_dtm_matrix), Novels_dtm_matrix[13, ], max.words = 70)
(head(sort(as.matrix(Novels_dtm)[13,], decreasing = TRUE), n=20))


#Distance Measures

m  <- Novels_dtm_matrix
m_norm <- Novels_Matrix_Norm
distMatrix_E <- dist(m, method="euclidean")
print(distMatrix_E)
distMatrix_C <- dist(m, method="cosine")
print(distMatrix_C)
distMatrix_C_norm <- dist(m_norm, method="cosine")

#Clustering 
# Hierarchical

## Euclidean
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.95, hang=-1)
rect.hclust(groups_E, k=4)

## Cosine Similarity
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=0.9, hang=-1)
rect.hclust(groups_C, k=5)

## Cosine Similarity for Normalized Matrix
groups_C_n <- hclust(distMatrix_C_norm,method="ward.D")
plot(groups_C_n, cex=0.9, hang=-1)
rect.hclust(groups_C_n, k=4)

#k means clustering
X <- m_norm

install.packages("factoextra")
library(factoextra)

distance1 <- get_dist(X,method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance2 <- get_dist(X,method = "pearson")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance3 <- get_dist(X,method = "canberra")
fviz_dist(distance3, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance4 <- get_dist(X,method = "spearman")
fviz_dist(distance4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


X <- t(X)
X <- scale(X)

str(X)
X <- t(X)
X <- scale(X)

str(X)
kmeansFIT_1 <- kmeans(X,centers=4)

summary(kmeansFIT_1)

fviz_cluster(kmeansFIT_1, data = X)

#Expectation Maximization

library(mclust)
ClusFI <- Mclust(X,G=4)
(ClusFI)
summary(ClusFI)
plot(ClusFI, what = "classification", dimens = c(1:10))
