library(bnlearn)
library(Rgraphviz)

#
#BiocManager::install("Rgraphviz")


#creates an extended GBN with random formulas per node
egbn.create = function(net, p_pwr, p_int){
  
  if (class(net) != "bn") {
    stop("'net' should be of class 'bn' ")
  }
  # vector of strings with the nodes in order..
  nodes <- node.ordering(net)
  
  # loop through the nodes
  for(node in nodes) {
    
    # get node object
    cn <- net$nodes[[node]]
    
    # We need the parentCount multiple times.
    parentCount <- length(cn$parents)
    
    # With k parents we need at least intercept + k coefs.
    coefs = sample(allowed_coefvalues,1+parentCount)
    
    # The intercept column name is set to "1", which makes it easier to generate 
    # a formula based on this named vector of coefficients.
    coefnames = c(c("1"),c(cn$parents))
    names(coefs) <- coefnames
    
    # With p_pwr chance, we add an power term of one the parents.
    p_pwr_sample =runif(1)
    if (parentCount>0 & p_pwr_sample<p_pwr) {
      
      # Pick one of the parents as a power term.
      pwr_node <- sample(cn$parents,1)
      
      # Add this node to the list of named coef
      coefs = append(coefs, sample(allowed_coefvalues,1))
      names(coefs)[length(coefs)] <- paste(pwr_node,"^2",sep="")
      
      # Also store the name of the power term inside the node for easy reference
      cn$powernode = pwr_node
    }
    
    p_int_sample = runif(1)
    if (parentCount>1 & p_int_sample<p_int) {
      
      # Pick two unique parents as an interaction term.
      int_nodes <- sample(cn$parents, size=2, replace =F)
      
      # Add this term to the list of named coef
      coefs = append(coefs, sample(allowed_coefvalues,1))
      names(coefs)[length(coefs)] <- paste(int_nodes,collapse = "*")
      
      # Also store the names of the interaction terms inside the node.
      cn$interactionnodes = int_nodes
      
    }
    
    #store the coefs and formula of the mean, into the node 
    cn$coefs <- coefs
    cn$mean  <- egbn.coefs2formula(coefs)
    
    #store the manipulated node back 
    net$nodes[[node]] <- cn
  }
  
  result <- net
}



#Converts a named vector of coefficients to an expression which can be evaluated.
egbn.coefs2formula = function(named_coefs){
  terms <- paste(named_coefs,names(named_coefs) , sep="*")
  result <- paste(terms, sep="", collapse=" + ")
}



#Draw samples from the eGBN as a dataframe
egbn.sample = function(egbn ,count=1, sd=1) 
{
  nc <-length(dag$nodes)
  
  # create empty dataframe of the correct size
  df <- data.frame( matrix(data=NA, 
                           nrow=count, 
                           ncol=nc, 
                           dimnames =list(NULL, names(egbn$nodes))))
  
  # fill each column with samples from N(mean, sd)
  # where mean is calculated based on the formula attached to each node.
  nodes <- node.ordering(egbn)
  for(node in nodes) {
    cn <- egbn$nodes[[node]]
    # parse the expression 
    f  <- parse(text=cn$mean)
    df[, node] <- rnorm(count, eval(f,envir=df),sd)
  }
  result <- df
}

egbn.printmodels = function(egbn)
{
  models <-lapply(myegbn$nodes, function (n) n$mean)
  for (n in names(models))
  {
    print(paste(n, " = ", models[n]))
  }
}

egbn.totalinteractions = function(egbn)
{
  tmp <-lapply(myegbn$nodes, function (n) if ( is.null(n[["interactionnodes"]])) { c(0) } else { c(1)})
  result <- sum(unlist(tmp))
}

egbn.totalpowers = function(egbn)
{
  tmp <-lapply(myegbn$nodes, function (n) if ( is.null(n[["powernode"]])) { c(0) } else { c(1)})
  result <- sum(unlist(tmp))
}
