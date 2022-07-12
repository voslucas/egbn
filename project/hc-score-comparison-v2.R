source("egbn-lib.R")
#this script compares different scores (aic-g, bic-g and bge)
#based on a random DAG which is then extend as a eGBN to draw a dataset
#dataset is used to run HillClimbing with different score functions
#and we finally compare the HammingDistance of the CPDAGs.
#we run this multiple times

#Change-log
# 4/7/2022 : hc(maxp = .. ) problem .. maxp requires max_degree+1 !!!
# 5/7/2022 : one dag + one trainset -> both HC methods in one call.

#Default experiment settings
nodecount <- 50   # number of nodes in the DAG
datasize <- 2000    # size of the train / test set
chance_int <- 0.0 # chance of an interaction in a node formula 
chance_pwr <- 0.0 # chance of a power term in a node formula
max_degree <- 3   # maximum degree of a node ( in and out)
my_sd <- 1.0

#max_degree_hc <- max_degree+1
max_degree_hc <- Inf

#Our columnnames 
columns = 1:nodecount
cnames = paste("x",columns, sep="")  # columns names are : x1,x2,x3, etc.

# Function defining a SINGLE experiment for a specified score used during HC
hcexperiment = function()
{
  # STEP 1 - create a DAG
  ldag = random.graph(cnames, 
                     num = 1, # For now, we only need one network ... 
                     method = "melancon", 
                     max.in.degree=max_degree, 
                     max.out.degree=max_degree )
  # STEP 2 - convert the DAG to an eGBN
  localegbn <- egbn.addmodels(ldag, chance_pwr, chance_int)
  # STEP 3 - draw samples from EGBN
  ltrainset <- egbn.sample(localegbn, datasize,my_sd)
  
  #Does it make a difference? 
  #- mean center?
  #ltrainset <- ltrainset[]-colMeans(ltrainset[])[col(ltrainset[])]
  #- auto scale , 0 mean , 1 sd
  #ltrainset <- as.data.frame(scale(ltrainset))
  

  # STEP 4 - quick structurelearning with simple existing HillClimb
  ltestdag_cus <- hc(ltrainset, 
                   maxp = max_degree_hc, 
                   score="custom", 
                   fun=egbn.customscore, 
                   optimized = TRUE)
  
  ltestdag_bic <- hc(ltrainset, 
                   maxp = max_degree_hc, 
                   score="bic-g", 
                   optimized = TRUE)
  
    
  # STEP 4B - probeer meer hillclimbs 
  
  # STEP 5 - get our metric 
  hd_cus <- hamming(ltestdag_cus,localegbn)
  hd_bic <- hamming(ltestdag_bic,localegbn)
  
  result <- c(hd_cus, hd_bic)
}

#NumberOfExperiment (noe) 
noe <- 100

#Run 100 times a single experiment for 3 differtent score functions. 

tmp <- replicate(noe, hcexperiment())
data <- data.frame( "custom" = tmp[1,] , "bicg" = tmp[2,])

print(nodecount)
print(datasize)
#Show statistics of these experiments
print(summary(data))
print(sapply(data, sd))



