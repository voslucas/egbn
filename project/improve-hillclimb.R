# improve HillClimb ?
# side project : 
# - why does hillclimb get stuck to local minima so easily?
source("egbn-lib.R")

#Experiment settings
nodecount <- 10   # number of nodes in the DAG
datasize <- 500   # size of the train / test set
chance_int <- 0.00 # chance of an interaction in a node formula 
chance_pwr <- 0.00 # chance of a power term in a node formula
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

# STEP 2a - convert the DAG to an eGBN
myegbn <- egbn.addmodels(dag, chance_pwr, chance_int)

trainset <- egbn.sample(myegbn, datasize, sd=1)

testdag <- hc(trainset, maxp = max_degree, score="bic-g")
print(score(testdag, trainset))
# STEP 5 - report hamming distance of the cpdags
starthd <- hamming(testdag,myegbn)


# STEP 6 - dig deeper by delete node?

test.iter = function( dag, data){
 
  mydag <- dag
  # verwijder de slechtse node?
  #mydag <- egbn.fit(dag,data)
  #mydag <- egbn.addscore(mydag,data)
  #x <- which.min(unlist(lapply(mydag$nodes, function (n) n$score)))
  #xname <- names(x)[1]
  #mydag <- remove.node(mydag,xname)
  #mydag <- add.node(mydag, xname)
  
  # verwijder drie random nodes?
  xname <- sample(names(mydag$nodes),1)
  mydag <- remove.node(mydag,xname)
  
  xname2 <- sample(names(mydag$nodes),1)
  mydag <- remove.node(mydag,xname2)
  
  xname3 <- sample(names(mydag$nodes),1)
  mydag <- remove.node(mydag,xname3)
  
  mydag <- add.node(mydag, xname)
  mydag <- add.node(mydag, xname2)
  mydag <- add.node(mydag, xname3)
  #swap twee nodes?
  
  
  #opnieuw HC?
  mydag <- hc(data,start=mydag, maxp = max_degree, score="bic-g")
  result <- mydag
}

test.run = function(){
  minscore <- -1E20
  bestdag <- testdag
  
  for (i in 1:0) {
    #print("----")
    tmpscore <- score(testdag, trainset)
    #print(tmpscore)
    #print(hamming(testdag,myegbn))
    
    if (tmpscore>minscore){
      minscore <- tmpscore
      bestdag <- testdag
      #print("newscore!")
    }
    
    testdag <- test.iter(bestdag, trainset)
  }
  
  print(starthd)
  print(hamming(bestdag,myegbn))
  
}

# different approach , start with empty graph. 
# tryout ALL new arcs, choose one with highest score?

test2.getallarcs = function(dag){
  nodenames = names(dag$nodes)
  fromlist = combn(names(testdag$nodes),2)[1,]
  tolist = combn(names(testdag$nodes),2)[2,]
  fromdef = c(fromlist,tolist)
  todef = c(tolist,fromlist)
  result <- list(from=fromdef, to=todef)
}

allarcs <- test2.getallarcs(dag)

test2.iter = function(dag, data){
  
  #list of all arcs?
  narcs <- length(allarcs$from)
  for (i in 1:narcs) {
    arc.st
  }
}



