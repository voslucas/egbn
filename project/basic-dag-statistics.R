# Determine the average HammingDistance of two random DAGs 
# both DAG's have exactly the number of nodes and same max_degree setting
# runs 100 times
# runs for numberofnodes from 10..100


source("egbn-lib.R")

# Global settings
max_degree <- 3   # maximum degree of a node ( in and out)
my_sd <- 1.0


experiment = function(nodecount) {
  columns = 1:nodecount
  cnames = paste("x",columns, sep="")  # columns names are : x1,x2,x3, etc.
  
  dag1 = random.graph(cnames, 
                      num = 1, # For now, we only need one network ... 
                      method = "melancon", 
                      max.in.degree=max_degree, 
                      max.out.degree=max_degree )
  
  dag2 = random.graph(cnames, 
                      num = 1, # For now, we only need one network ... 
                      method = "melancon", 
                      max.in.degree=max_degree, 
                      max.out.degree=max_degree )
  
  result <- hamming(dag1,dag2)
}


ncs = 10:20

avghd = function(n) {
  tmp = replicate(100, experiment(n), simplify = "array")
  result <- mean(tmp)
}

ncs2 = lapply(ncs,avghd)
print(ncs2)



