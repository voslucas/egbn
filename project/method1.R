# method 1  - rommel project om method 1 te proberen.
# Final version 
# get random DAG
# extend it with models
# get datasets
# normal HC method
# get bic-g and likelihood
# per node with parents
#   look for better models
#   if found, add those nodes to the dataset + DAG 
# refit all parameters.
# get bic-g and likelihood again.
library(glmnet)

source("egbn-lib.R")

#Experiment settings
nodecount <- 10   # number of nodes in the DAG
datasize <- 500   # size of the train / test set
chance_int <- 0.20 # chance of an interaction in a node formula 
chance_pwr <- 0.20 # chance of a power term in a node formula
max_degree <-3    # maximum degree of a node ( in and out)

#Our columnnames 
columns = 1:nodecount
cnames = paste("x",columns, sep="")  # columns names are : x1,x2,x3, etc.

allowed_coefvalues <- 1:10    # we use 1..10 instead of the proposed -10..10 for now


# STEP 1 - create a DAG
dag = random.graph(cnames, 
                   num = 1, # For now, we only need one network ... 
                   method = "melancon", 
                   max.in.degree=max_degree, 
                   max.out.degree=max_degree )

# Optional : show the graph. 
if (nodecount<=50) { graphviz.plot(dag) }


# STEP 2a - convert the DAG to an eGBN
myegbn <- egbn.addmodels(dag, chance_pwr, chance_int)

# STEP 2b - report the constructed models
print(modelstring(myegbn))
print( paste("Total interaction terms in eGBN: ", egbn.totalinteractions(myegbn)))
print( paste("Total       power terms in eGBN: ", egbn.totalpowers(myegbn)))
egbn.printmodels(myegbn)


# STEP 3 - draw samples from EGBN
trainset <- egbn.sample(myegbn, datasize, sd=1)
testset <- egbn.sample(myegbn, datasize , sd=1)

trainset_scaled <- as.data.frame(scale(trainset))

# STEP 4 - do a quick structurelearning with simple existing HillClimb
testdag <- hc(trainset_scaled, maxp = max_degree, score="bic-g")
print(score(testdag,trainset))
print(score(testdag, testset))

# STEP 5- per node (with parents) , search for better extend model.

bettermodel <- function(node, dag, dataset)
{
  
  #select data all parents + node itset
  tmpset <- dataset[dag$nodes[node]$parents]
}

bettermodel("x3", testdag, testset)

# eerst met het handje maar eens proberen.. 

node <-"x10"
tmpset <- testset[myegbn$nodes[[node]]$parents]
tmpset <- egbn.augmentdata(tmpset)

pc <- length(myegbn$nodes[[node]]$parents)

sourcecolumns <- colnames(tmpset)
# breid deze tijdelijk dataset uit met ook de node data.
# deze moet als target dienen tenslotte.
tmpset[node] <- testset[node]

# bestaat er een model wat beter past?
cv.glmnet(x=data.matrix(rep(1,500)),y=data.matrix(tmpset[node]), alpha=1)
# zoek naar de lamba
cv_model <- cv.glmnet(data.matrix(tmpset[sourcecolumns]), 
                      data.matrix(tmpset[node]), alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

best_model <- glmnet(data.matrix(tmpset[sourcecolumns]), 
                     data.matrix(tmpset[node]), alpha = 1, 
                     lambda = best_lambda)

coef(best_model)
logLik(best_model)
class(coef(best_model))

c4 <- coef(best_model)[,1]
c5 <- c4[c4!=0]

c2 <- c[,1]
c3 <- c2[ c2!=0]
names(c3)[1] <- "1"
names(c3)

c4 <- egbn.coefs2formula(c3)
c4

eval(parse(text=c4),envir=testset)


best_model_lm <- lm( formula = x8 ~ x3+x3x3, data=tmpset)


#predict(best_model, newx= c(x3=3752,x3x3=3752*3752))
#means <- unlist(as.list(predict(best_model, 
                       newx= data.matrix(tmpset[1:2]))))

tmpset["x8"]
dnorm()

dnorm(42886349, mean = means[1], sd=sd(means))
sd(means)

