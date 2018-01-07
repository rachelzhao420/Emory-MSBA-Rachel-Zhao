# Emory-MSBA-Rachel-Zhao
# A part of code that I did for Social Network Homework
rm(list = ls(all = TRUE))
library(igraph)
library(data.table)
library(gdata)
library('lsa')
library(XML)
library(plyr)
library(sets)
setwd("C:/Users/yang jian/Desktop/Emory/ISOM 673/hw4")
df <- read.csv("C:/Users/yang jian/Desktop/Emory/ISOM 673/hw4/production_keyword_matrix_1985.csv")
View(df)
typeof(df)  #list


matrixdf<- data.matrix(df, rownames.force = NA)
typeof(matrixdf)  #integer
View(matrixdf)

similarity <- function(x) 
{
  A = matrix[,x[1]]
  B = matrix[,x[2]]
  
  return( sum(A*B)/(sqrt(sum(A^2)))*(sqrt(sum(B^2))) )
}   
n <- ncol(matrixdf) 
cmb <- expand.grid(i=1:n, j=1:n) 
C <- as.numeric(matrix(apply(cmb,1,similarity),n,n))  
#answer for Cosine similarity is 70




#M1<- set(df)
#SimCos<-similarity(M1, method="jaccard")

jac_similarity <- function(i,j){
    A <- matrixdf[,i]%*%matrixdf[,j]
    B<- sum(matrixdf[,i])
    c<- sum(matrixdf[,j])
    jac_sim<- a/(b+c-a)
    return(jac_similarity)
}

for(i in 1:nrow(cmb)){
  cmb$jaccard[i]<-matrix(cmb[i,1],cmb[i,2])
}

length(which(cmb$jaccard==1))    
#answer for Jaccard similarity is 292
