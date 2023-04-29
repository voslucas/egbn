#(c) Lucas Vos - 2022

#Run a datacollection
#Experimental settings come from the commandline args
#VERSION 2.0

source("egbn-lib.R")
library(rjson)
library(optigrab)


# number of nodes in the DAG
nodecount <- opt_get("nodecount", default=50, required=TRUE)
# size of the trainset
datasize <- opt_get("datasize", default=500,required=TRUE)
# chance of an interaction in a node formula 
chance_int <- opt_get("pint", default=0.0,required=TRUE)
# chance of a power term in a node formula
chance_pwr <- opt_get("ppwr",default=0.0,required=TRUE)
# maximum degree of a node ( in and out)
max_degree <- opt_get("degree", default=5, required=TRUE)
#
mysd  <- opt_get("sd", default=0.5, required=TRUE)

# when learning a DAG we specify a max degree
max_degree_hc <- Inf

#Global experimental settings
do_balance <- FALSE

#Our columnnames 
columns = 1:nodecount
cnames = paste("x",columns, sep="")  # columns names are : x1,x2,x3, etc.

#output stores all relevant information about this experiment
output <- list()
output$nodecount = nodecount
output$datasize = datasize
output$chance_int = chance_int
output$chance_pwr = chance_pwr
output$max_degree = max_degree
output$samplemethod = 1
output$version = 2

# STEP 1 - create a DAG
dag = random.graph(cnames, 
                   num = 1, # For now, we only need one network ... 
                   method = "melancon", 
                   max.in.degree=max_degree, 
                   max.out.degree=max_degree )

# STORE the generated DAG for later analysis
output$dag = modelstring(dag)

# STEP 2a - convert the DAG to an eGBN
start_time = Sys.time()
myegbn = egbn.addmodels(dag, chance_pwr, chance_int)
output$timing_addmodels = as.numeric(Sys.time()-start_time, units="secs")

# STEP 2b - report the constructed models
#print(modelstring(myegbn))
output$totalinteractions = egbn.totalinteractions(myegbn)
output$totalpowers = egbn.totalpowers(myegbn)
output$interactionnodes =egbn.getnodeswithinteraction(myegbn)
output$powernodes=egbn.getnodeswithpower(myegbn)
print( paste("Total interaction terms in eGBN: ", egbn.totalinteractions(myegbn)))
print( paste("Total       power terms in eGBN: ", egbn.totalpowers(myegbn)))
#egbn.printmodels(myegbn)


# STEP 3 - draw samples from EGBN
start_time = Sys.time()
trainset <- egbn.sample(myegbn,datasize,mysd)
testset <- egbn.sample(myegbn,datasize,mysd)
output$timing_generatetrainset = as.numeric(Sys.time()-start_time, units="secs")

print("ground scores on trainset")
start_time = Sys.time()
fitted <- egbn.fitinline(myegbn, trainset,  method="lm" , augment = FALSE)
output$ground_loglik     = egbn.score(fitted ,testset)
output$ground_bicg       = egbn.score(fitted, testset,  method= "bic-g")
output$ground_bicg_train = egbn.score(fitted, trainset, method= "bic-g")
output$ground_arcs = length(arcs(myegbn))
output$ground_mb   = egbn.mb(myegbn)
output$ground_nbr  = egbn.nbr(myegbn)
output$ground_bf   = egbn.branchfactor(myegbn)
output$timing_ground = as.numeric(Sys.time()-start_time, units="secs")

# STEP 4 - do a quick structurelearning with simple existing HillClimb
start_time = Sys.time()
hc0 <- hc(trainset, maxp = max_degree_hc, score="bic-g")
output$hc0_arcs = length(arcs(hc0))
output$hc0_mb   = egbn.mb(hc0)
output$hc0_nbr  = egbn.nbr(hc0)
output$hc0_bf   = egbn.branchfactor(hc0)
output$hc0_hd = hamming(hc0,myegbn)
output$hc0_freenodes = length(egbn.getfreenodes(hc0))
output$timing_hc0 = as.numeric(Sys.time()-start_time, units="secs")

# METHOD0 = HC0 + lm/no_augment  

#LM FIT , no augmentation
start_time = Sys.time()
fitted <- egbn.fitinline(hc0, trainset,  method="lm" , augment = FALSE)
output$m0_loglik     = egbn.score(fitted, testset )
output$m0_bicg       = egbn.score(fitted, testset, method = "bic-g")
output$m0_bicg_train = egbn.score(fitted, trainset, method = "bic-g")
output$m0_k          = egbn.getk(fitted)
output$timing_m0 = as.numeric(Sys.time()-start_time, units="secs")

# METHOD1 = HC0 + glm/aug  

#GLM FIT ,aug 
start_time = Sys.time()
fitted <- egbn.fitinline(hc0, trainset,  method="glm", augment = TRUE)
output$m1_loglik     = egbn.score(fitted, testset )
output$m1_bicg       = egbn.score(fitted, testset, method = "bic-g")
output$m1_bicg_train = egbn.score(fitted, trainset, method = "bic-g")
output$m1_k          = egbn.getk(fitted)
output$m1_interactionnodes = egbn.getnodeswithinteraction(fitted)
output$m1_powernodes       = egbn.getnodeswithpower(fitted)
output$timing_m1 = as.numeric(Sys.time()-start_time, units="secs")


# STEP 5 - probeer nu een HillClimb met een Custom score functie.
start_time = Sys.time()
hc2 <- hc(trainset, maxp = max_degree_hc, score="custom" , fun=egbn.customscore)
output$hc2_arcs = length(arcs(hc2))
output$hc2_mb   = egbn.mb(hc2)
output$hc2_nbr  = egbn.nbr(hc2)
output$hc2_bf   = egbn.branchfactor(hc2)
output$hc2_hd = hamming(hc2,myegbn)
output$hc2_freenodes = length(egbn.getfreenodes(hc2))
output$timing_hc2 = as.numeric(Sys.time()-start_time, units="secs")


# METHOD2 = HC2 + lm/no_augment  

#LM FIT , no augmentation
start_time = Sys.time()
fitted <- egbn.fitinline(hc2, trainset,  method="lm" , augment = FALSE)
output$m2_loglik     = egbn.score(fitted, testset )
output$m2_bicg       = egbn.score(fitted, testset, method = "bic-g")
output$m2_bicg_train = egbn.score(fitted, trainset, method = "bic-g")
output$m2_k          = egbn.getk(fitted)
output$timing_m2     = as.numeric(Sys.time()-start_time, units="secs")

# METHOD3 = HC2 + glm/aug  

#GLM FIT ,aug 
start_time = Sys.time()
fitted <- egbn.fitinline(hc2, trainset,  method="glm", augment = TRUE)
output$m3_loglik     = egbn.score(fitted, testset )
output$m3_bicg       = egbn.score(fitted, testset, method = "bic-g")
output$m3_bicg_train = egbn.score(fitted, trainset, method = "bic-g")
output$m3_k          = egbn.getk(fitted)
output$m3_interactionnodes = egbn.getnodeswithinteraction(fitted)
output$m3_powernodes       = egbn.getnodeswithpower(fitted)
output$timing_m3           = as.numeric(Sys.time()-start_time, units="secs")


#OUTPUT 
#For easy parsing the results we add the +++ before the json starts.
print("+++")
print(toJSON(output))