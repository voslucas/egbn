#Run a datacollection
#Experimental settings come from the commandline args

source("egbn-lib.R")
library(rjson)
library(optigrab)


# number of nodes in the DAG
nodecount <- opt_get("nodecount", default=10, required=TRUE)
# size of the trainset
datasize <- opt_get("datasize", default=5000,required=TRUE)
# chance of an interaction in a node formula 
chance_int <- opt_get("pint", default=0.0,required=TRUE)
# chance of a power term in a node formula
chance_pwr <- opt_get("ppwr",default=0.0,required=TRUE)
# maximum degree of a node ( in and out)
max_degree <- opt_get("degree", default=3, required=TRUE)
#
mysd  <- opt_get("sd", default=0.5, required=TRUE)

# when learning a DAG we specify a max degree
max_degree_hc <- 10

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
output$timing_generatetrainset = as.numeric(Sys.time()-start_time, units="secs")

#STEP 3B - mean center + scale dataset
if (do_balance) {
  trainset <- as.data.frame(scale(trainset))
}

print("ground scores on trainset")
start_time = Sys.time()
output$ground_bicg = score(myegbn, trainset)
output$ground_loglik = score(myegbn ,trainset, type="loglik-g")
print(output$ground_bicg)
output$timing_ground = as.numeric(Sys.time()-start_time, units="secs")

# STEP 4 - do a quick structurelearning with simple existing HillClimb
start_time = Sys.time()
testdag1 <- hc(trainset, maxp = max_degree_hc, score="bic-g", restart=1)
output$timing_hc0 = as.numeric(Sys.time()-start_time, units="secs")

print("recoverd score on trainset with bic-g")
start_time = Sys.time()
output$method0_bicg = score(testdag1, trainset)
output$method0_loglik = score(testdag1 ,trainset, type="loglik-g")
output$method0_hd = hamming(testdag1,myegbn)
output$method0_freenodes = length(egbn.getfreenodes(testdag1))
output$timing_method0 = as.numeric(Sys.time()-start_time, units="secs")

print(output$method0_bicg)
print(output$method0_hd)


# METHOD1 
# Check if a better model fits on the default learned/recovered 

#LM FIT , no augmentation // should result in the same results as above.
start_time = Sys.time()
fitted <- egbn.fit(testdag1, trainset,  method="lm" , augment = FALSE)
output$method1_lm_noaug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_lm_noaug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_lm_noaug_k = egbn.getk(fitted)
output$timing_method1_lm_noaug_k = as.numeric(Sys.time()-start_time, units="secs")

#GLM fit , no augment
start_time = Sys.time()
fitted <- egbn.fit(testdag1, trainset,  method="glm", augment = FALSE)
output$method1_glm_noaug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_glm_noaug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_glm_noaug_k = egbn.getk(fitted)
output$timing_method1_glm_noaug_k = as.numeric(Sys.time()-start_time, units="secs")

#LM fit ,  augmented
start_time = Sys.time()
fitted <- egbn.fit(testdag1, trainset,  method="lm" , augment = TRUE)
output$method1_lm_aug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_lm_aug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_lm_aug_k = egbn.getk(fitted)
output$timing_method1_lm_aug_k = as.numeric(Sys.time()-start_time, units="secs")


#GLM FIT ,aug 
start_time = Sys.time()
fitted <- egbn.fit(testdag1, trainset,  method="glm", augment = TRUE)
output$method1_glm_aug_loglik = egbn.score(fitted, trainset, debug= FALSE)
output$method1_glm_aug_bicg = egbn.score(fitted, trainset, method = "bic-g" , debug= FALSE)
output$method1_glm_aug_k = egbn.getk(fitted)
output$method1_glm_aug_interactionnodes =egbn.getnodeswithinteraction(fitted)
output$method1_glm_aug_powernodes=egbn.getnodeswithpower(fitted)
output$timing_method1_glm_aug_k = as.numeric(Sys.time()-start_time, units="secs")

# METHOD2
# Learn with Custom Score functie , in 2 varianten. 1 lm en 1 glm gebaseerd.

# STEP 5 - probeer nu een HillClimb met een custom score functie.
start_time = Sys.time()
testdag2 <- hc(trainset, maxp = max_degree_hc, score="custom" , fun=egbn.customscore , restart=1)
output$timing_hc2a = as.numeric(Sys.time()-start_time, units="secs")

output$method2_hd = hamming(testdag2,myegbn)
output$method2_freenodes = length(egbn.getfreenodes(testdag2))

#LM FIT , no augmentation
start_time = Sys.time()
fitted2 <- egbn.fit(testdag2, trainset,  method="lm" , augment = FALSE)
output$method2_lm_noaug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_lm_noaug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_lm_noaug_k = egbn.getk(fitted2)
output$timing_method2_lm_noaug_k = as.numeric(Sys.time()-start_time, units="secs")

#GLM fit , no augment
start_time = Sys.time()
fitted2 <- egbn.fit(testdag2, trainset,  method="glm", augment = FALSE)
output$method2_glm_noaug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_glm_noaug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_glm_noaug_k = egbn.getk(fitted2)
output$timing_method2_glm_noaug_k = as.numeric(Sys.time()-start_time, units="secs")

#LM fit ,  augmented
start_time = Sys.time()
fitted2 <- egbn.fit(testdag2, trainset,  method="lm" , augment = TRUE)
output$method2_lm_aug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_lm_aug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_lm_aug_k = egbn.getk(fitted2)
output$timing_method2_lm_aug_k = as.numeric(Sys.time()-start_time, units="secs")

#GLM FIT ,aug 
start_time = Sys.time()
fitted2 <- egbn.fit(testdag2, trainset,  method="glm", augment = TRUE)
output$method2_glm_aug_loglik = egbn.score(fitted2, trainset, debug= FALSE)
output$method2_glm_aug_bicg = egbn.score(fitted2, trainset, method = "bic-g" , debug= FALSE)
output$method2_glm_aug_k = egbn.getk(fitted2)
output$method2_glm_aug_interactionnodes =egbn.getnodeswithinteraction(fitted2)
output$method2_glm_aug_powernodes=egbn.getnodeswithpower(fitted2)
output$timing_method2_glm_aug= as.numeric(Sys.time()-start_time, units="secs")

# METHOD2-B
# Learn with Custom Score functie the GLM variant. 

if (nodecount<=10)
{
  start_time = Sys.time()
  testdag3 <- hc(trainset, maxp = max_degree_hc, score="custom" , fun=egbn.customscore_glm , restart=1)
  output$timing_hc2b = as.numeric(Sys.time()-start_time, units="secs")
  output$method2b_hd = hamming(testdag3,myegbn)
  output$method2b_freenodes = length(egbn.getfreenodes(testdag3))
  
  #GLM FIT ,aug of Method2-b ; other methods are not useful.
  start_time = Sys.time()
  fitted3 <- egbn.fit(testdag3, trainset,  method="glm", augment = TRUE)
  output$method2b_glm_aug_loglik = egbn.score(fitted3, trainset, debug= FALSE)
  output$method2b_glm_aug_bicg = egbn.score(fitted3, trainset, method = "bic-g" , debug= FALSE)
  output$method2b_glm_aug_k = egbn.getk(fitted3)
  output$method2b_glm_aug_interactionnodes =egbn.getnodeswithinteraction(fitted3)
  output$method2b_glm_aug_powernodes=egbn.getnodeswithpower(fitted3)
  output$timing_method2b_glm_aug= as.numeric(Sys.time()-start_time, units="secs")
}


#OUTPUT 
#For easy parsing the results we add the +++ before the json starts.
print("+++")
print(toJSON(output))