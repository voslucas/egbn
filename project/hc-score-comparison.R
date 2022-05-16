source("egbn-lib.R")
#this script compares different scores (aic-g, bic-g and bge)
#based on a random DAG which is then extend as a eGBN to draw a dataset
#dataset is used to run HillClimbing with different score functions
#and we finally compare the HammingDistance of the CPDAGs.
#we run this multiple times


#Default experiment settings
nodecount <- 10   # number of nodes in the DAG
datasize <- 500   # size of the train / test set
chance_int <- 0.00 # chance of an interaction in a node formula 
chance_pwr <- 0.00 # chance of a power term in a node formula
max_degree <-3    # maximum degree of a node ( in and out)

#Our columnnames 
columns = 1:nodecount
cnames = paste("x",columns, sep="")  # columns names are : x1,x2,x3, etc.

allowed_coefvalues <- 1:10   # we use 1..10 instead of the proposed -10..10 for now


# Function defining a SINGLE experiment for a specified score used during HC
hcexperiment = function(scorename)
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
  ltrainset <- egbn.sample(localegbn, datasize, sd=1)
  
  #Does it make a difference? 
  #- mean center?
  #ltrainset <- ltrainset[]-colMeans(ltrainset[])[col(ltrainset[])]
  #- auto scale , 0 mean , 1 sd
  #ltrainset <- as.data.frame(scale(ltrainset))
  

  # STEP 4 - quick structurelearning with simple existing HillClimb
  ltestdag <- hc(ltrainset, maxp = max_degree, score=scorename)
  # STEP 5 - get our metric 
  hammingdistance <- hamming(ltestdag,localegbn)
  
  result <- hammingdistance
}

#NumberOfExperiment (noe) 
noe <- 100

#Run 100 times a single experiment for 3 differtent score functions. 
data <- data.frame( "aic-g"=replicate(noe, hcexperiment("aic-g"), simplify="array"),
                    "bic-g"=replicate(noe, hcexperiment("bic-g"), simplify="array"),
                    "bge"  =replicate(noe, hcexperiment("bge"), simplify="array")
                  )

#Show statistics of these experiments
print(summary(data))
print(sapply(data, sd))


#Conclusion -> bge finds best fit for HammingDistance



