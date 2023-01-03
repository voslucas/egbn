
source("egbn-lib.R")

# Global settings
max_degree <- 3   # maximum degree of a node ( in and out)
my_sd <- 0.5

# How does trainset size / sample size effect Hamming Distance?

# Second experiment is:
# for a single random DAG
# add some linear models to the nodes
# draw sample sets from this eGBN .. of different size
# use default HillClimbing, single run, to recover the DAG for each 
# sample size
# run XX experiments 

experiment2 = function(nodecount){
  columns = 1:nodecount
  cnames = paste("x",columns, sep="")  # columns names are : x1,x2,x3, etc.
  
  dag3 = random.graph(cnames, 
                      num = 1, # For now, we only need one network ... 
                      method = "melancon", 
                      max.in.degree=max_degree, 
                      max.out.degree=max_degree )
  
  myegbn = egbn.addmodels(dag3,0,0)
  
  #generate different sample size
  samplesizes = list(10,50,100,500,1000,5000,10000)
  
  sets = lapply(samplesizes, function(ss) egbn.sample(myegbn,ss,my_sd) )
  
  newdags = lapply(sets, function (s) hc(s, score="bic-g"))
  
  hds = lapply(newdags, function (d) hamming(d,dag3))
  
  result <- unlist(hds)
  
}

samplesize_experiment = replicate(500, experiment2(10))

#breng de [1:7] [1:100] dimensis terug..

rows = 1:7
samplesize_experiment_mean= lapply(rows, function(r) mean(samplesize_experiment[r,]) )


print(unlist(samplesize_experiment_mean))
