#experimental setup to getter data

source("egbn-lib.R")
library(rjson)

args = commandArgs(trailingOnly=TRUE)
print(paste0("args are:", args))


#Experiment settings
id <- 1
sd <- 1
nodecount <- 10   # number of nodes in the DAG
datasize <- 500   # size of the train / test set
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
#print( paste("Total interaction terms in eGBN: ", egbn.totalinteractions(myegbn)))
#print( paste("Total       power terms in eGBN: ", egbn.totalpowers(myegbn)))
#egbn.printmodels(myegbn)


# STEP 3 - draw samples from EGBN
trainset <- egbn.sample(myegbn, datasize, sd=sd)

#STEP 3B - mean center + scale dataset
#trainset <- as.data.frame(scale(trainset))

print("ground scores on trainset")
output$ground_bicg = score(myegbn, trainset)
output$ground_loglik = score(myegbn ,trainset, type="loglik-g")
#print(score(myegbn, trainset))

# STEP 4 - do a quick structurelearning with simple existing HillClimb
testdag <- hc(trainset, maxp = max_degree, score="bic-g", restart=1)

print("recoverd score on trainset with bic-g")
print(score(testdag, trainset))
print(hamming(testdag,myegbn))
output$method0_bicg = score(testdag, trainset)
output$method0_loglik = score(testdag ,trainset, type="loglik-g")
output$method0_hd = hamming(testdag,myegbn)

# METHOD1 



# STEP 5 - probeer nu een HillClimb met een custom score functie.
testdag <- hc(trainset, maxp = max_degree, score="custom" , fun=egbn.customscore , restart=1)
print("recoverd score on trainset with custom hc method and normal fit")
print(score(testdag, trainset))
print(hamming(testdag,myegbn))

print("recoverd score on trainset with custom hc method and custom fit")
testdag <- egbn.fit(testdag,trainset,augment = TRUE)
print(egbn.score(testdag,trainset))
print(egbn.score(testdag,trainset, method="bic-g",debug=FALSE))

print(egbn.getk(testdag))
