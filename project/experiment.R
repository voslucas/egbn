source("egbn-lib.R")

#Experiment settings
nodecount <- 10   # number of nodes in the DAG
datasize <- 5000   # size of the train set
chance_int <- 0.1 # chance of an interaction in a node formula 
chance_pwr <- 0.1 # chance of a power term in a node formula
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


# STEP 2a - convert DAG to eGBN
myegbn <- egbn.create(dag, chance_pwr, chance_int)

# STEP 2b - reportthe constructed models

print( paste("Total interaction terms in eGBN: ", egbn.totalinteractions(myegbn)))
print( paste("Total       power terms in eGBN: ", egbn.totalpowers(myegbn)))
egbn.printmodels(myegbn)



# STEP 3 - draw samples from EGBN
trainset <- egbn.sample(myegbn, datasize)
testset <- egbn.sample(myegbn, datasize)

# STEP 4 - quick test with simple existing HillClimb

testdag <- hc(trainset, maxp = max_degree)
score(testdag,trainset)
score(testdag, testset)


if (nodecount<=50) { graphviz.plot(testdag) }
