#experimental setup to getter data

source("egbn-lib.R")
library(rjson)

args = commandArgs(trailingOnly=TRUE)
print(paste0("args are:", args))

#Experiment settings
id <- 1
mysd <- 0.1
nodecount <- 1000   # number of nodes in the DAG
datasize <- 5000   # size of the train / test set
chance_int <- 0.25 # chance of an interaction in a node formula 
chance_pwr <- 0.25 # chance of a power term in a node formula
max_degree <- 3   # maximum degree of a node ( in and out)
do_balance <- FALSE # 

#Our columnnames 
columns = 1:nodecount
cnames = paste("x",columns, sep="")  # columns names are : x1,x2,x3, etc.

# STEP 1 - create a DAG
dag = random.graph(cnames, 
                   num = 1, # For now, we only need one network ... 
                   method = "melancon", 
                   max.in.degree=max_degree, 
                   max.out.degree=max_degree )

# STORE the generated DAG for later analysis
write(toJSON(dag),file= paste0("dag-",id,".json"))

# Optional : show the graph. 
# Skip display, we only want data
# if (nodecount<=50) { graphviz.plot(dag) }


# STEP 2a - convert the DAG to an eGBN
myegbn = egbn.addmodels(dag, chance_pwr, chance_int)

# STEP 2b - report the constructed models
#print(modelstring(myegbn))
output <- list()
output$totalinteractions = egbn.totalinteractions(myegbn)
output$totalpowers = egbn.totalpowers(myegbn)
print( paste("Total interaction terms in eGBN: ", egbn.totalinteractions(myegbn)))
print( paste("Total       power terms in eGBN: ", egbn.totalpowers(myegbn)))
#egbn.printmodels(myegbn)

# STEP 3 - draw samples from EGBN
trainset <- egbn.sample(myegbn,datasize,mysd)

stop("even")

#STEP 3B - mean center + scale dataset
if (do_balance) {
  trainset <- as.data.frame(scale(trainset))
}

print("ground scores on trainset")
output$ground_bicg = score(myegbn, trainset)
output$ground_loglik = score(myegbn ,trainset, type="loglik-g")
print(score(myegbn, trainset))

# STEP 4 - do a quick structurelearning with simple existing HillClimb
testdag1 <- hc(trainset, maxp = max_degree, score="bic-g", restart=1)

print("recoverd score on trainset with bic-g")
print(score(testdag1, trainset))
print(hamming(testdag1,myegbn))
output$method0_bicg = score(testdag1, trainset)
output$method0_loglik = score(testdag1 ,trainset, type="loglik-g")
output$method0_hd = hamming(testdag1,myegbn)
output$method0_freenodes = length(egbn.getfreenodes(testdag1))

# METHOD1 
# Check if a better model fits on the default learned/recovered 

#LM FIT , no augmentation // should result in the same results as above.
fitted <- egbn.fit(testdag1, trainset,  method="lm" , augment = FALSE)
output$method1_lm_noaug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_lm_noaug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_lm_noaug_k = egbn.getk(fitted)

#GLM fit , no augment
fitted <- egbn.fit(testdag1, trainset,  method="glm", augment = FALSE)
output$method1_glm_noaug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_glm_noaug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_glm_noaug_k = egbn.getk(fitted)

#LM fit ,  augmented
fitted <- egbn.fit(testdag1, trainset,  method="lm" , augment = TRUE)
output$method1_lm_aug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_lm_aug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_lm_aug_k = egbn.getk(fitted)

#GLM FIT ,aug 
fitted <- egbn.fit(testdag1, trainset,  method="glm", augment = TRUE)
output$method1_glm_aug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_glm_aug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_glm_aug_k = egbn.getk(fitted)

# METHOD2
# Learn with Custom Score functie , in 2 varianten. 1 lm en 1 glm gebaseerd.

# STEP 5 - probeer nu een HillClimb met een custom score functie.
testdag2 <- hc(trainset, maxp = max_degree, score="custom" , fun=egbn.customscore , restart=1)

output$method2_hd = hamming(testdag2,myegbn)
output$method2_freenodes = length(egbn.getfreenodes(testdag2))

#LM FIT , no augmentation
fitted2 <- egbn.fit(testdag2, trainset,  method="lm" , augment = FALSE)
output$method2_lm_noaug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_lm_noaug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_lm_noaug_k = egbn.getk(fitted2)

#GLM fit , no augment
fitted2 <- egbn.fit(testdag2, trainset,  method="glm", augment = FALSE)
output$method2_glm_noaug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_glm_noaug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_glm_noaug_k = egbn.getk(fitted2)

#LM fit ,  augmented
fitted2 <- egbn.fit(testdag2, trainset,  method="lm" , augment = TRUE)
output$method2_lm_aug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_lm_aug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_lm_aug_k = egbn.getk(fitted2)

#GLM FIT ,aug 
fitted2 <- egbn.fit(testdag2, trainset,  method="glm", augment = TRUE)
output$method2_glm_aug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_glm_aug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_glm_aug_k = egbn.getk(fitted2)






