source("egbn-lib.R")

#Experiment settings
nodecount <- 10   # number of nodes in the DAG
datasize <- 500   # size of the train / test set
chance_int <- 0.45 # chance of an interaction in a node formula 
chance_pwr <- 0.25 # chance of a power term in a node formula
max_degree <- 3   # maximum degree of a node ( in and out)

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
#egbn.printmodels(myegbn)
 

# STEP 3 - draw samples from EGBN
trainset <- egbn.sample(myegbn, datasize, sd=1)
testset <- egbn.sample(myegbn, datasize , sd=1)


# STEP 4 - do a quick structurelearning with simple existing HillClimb
testdag <- hc(trainset, maxp = max_degree, score="bic-g")
print("recoverd score on trainset")
print(score(testdag, trainset))
print("ground   score on trainset")
print(score(myegbn, trainset))



# STEP 5 - report hamming distance of the cpdags
print("hamdistance on recoverd vs original dag")
print(hamming(testdag,myegbn))


# STEP 6 - test custom fit and score functions on LM 

print("- LM fit , no augment -")
fitted <- egbn.fit(testdag, trainset, augment = FALSE)
print(egbn.score(fitted, trainset, debug= FALSE))
print(egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE))


print("- GLM fit , no augment -")
fitted <- egbn.fit(testdag, trainset, method="glm", augment = FALSE)
print(egbn.score(fitted, trainset, debug= FALSE))
print(egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE))

print("- LM fit ,  augment -")
fitted <- egbn.fit(testdag, trainset, augment = TRUE)
print(egbn.score(fitted, trainset, debug= FALSE))
print(egbn.score(fitted, trainset, method = "bic-g", debug= FALSE))

print("- GLM fit ,augment -")
fitted <- egbn.fit(testdag, trainset, method="glm", augment = TRUE)
print(egbn.score(fitted, trainset, debug= FALSE))
print(egbn.score(fitted, trainset, method = "bic-g", debug= FALSE))

#print(score(testdag, trainset, type="loglik-g" ,debug=FALSE))
#print(score(testdag, trainset, type="bic-g" ,debug=FALSE))
#print(egbn.score(fitted, trainset, method = "bic-g",debug= FALSE))

