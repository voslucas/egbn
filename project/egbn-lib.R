library(bnlearn)
library(Rgraphviz)
library(glmnet)

#Unused ; idea was to cache EGBN 
cachedegbndata__ = NULL

allowed_coefvalues <- -9:9
allowed_coefvalues <- allowed_coefvalues[allowed_coefvalues!=0]

#Get n randomcoefs  
egbn.randomcoefs = function(n){
  #Note: we initial sampled from nice integer values 1:10 
  #      but this results in Infite values on large networks
  
  result <- sample(allowed_coefvalues,n) / 10.0

}

# return a bic score of a LM based on augmented data
egbn.customscore = function(node, parents, data, args) {

     workdata = data[parents]
     workdata = egbn.augmentdata(workdata)
     
     if (length(parents) == 0)
          model = as.formula(paste(node, "~ 1"))     
     else     
          model = as.formula(paste(node, "~", paste(names(workdata), collapse = "+")))
    
     #extend workdata with node data
     workdata[node] = data[node]
     result <- -BIC(lm(model, data = workdata)) / 2.0
}



# TODO :this is SLOW.. 
# it would benefit from an cached precalculated augmented dataset
egbn.customscore_glm = function(node, parents, data, args) 
{
  
  if (length(parents)<=1) {
    result <- egbn.customscore(node,parents,data, args)
  } else
  {
    workdata = data[parents]
    workdata = egbn.augmentdata(workdata)
    dcols = names(workdata)
    workdata[node] = data[node]
    
    cv_model <- cv.glmnet(data.matrix(workdata[dcols]), 
                          data.matrix(workdata[node]), alpha = 1)
    
    #find optimal lambda value that minimizes test MSE
    best_lambda <- cv_model$lambda.min
    
    best_model <- glmnet(data.matrix(workdata[dcols]), 
                         data.matrix(workdata[node]), alpha = 1, 
                         lambda = best_lambda, keep=TRUE)
    
    # BIC's / residuals etc. are not directly available for this model
    # but we have done "variable selection"  based on LASSO ..
    # we use these selected nodes to build a modelstring 
    # but simply use the existing BIC/LM 
    
    ctemp <- coefficients(best_model)[,1]
    #remove unused 
    ctempfilter <- ctemp[ctemp!=0]
    #replace the term Intercept, if needed. (sometimes there is not intercept)
    if (names(ctempfilter)[1] == "(Intercept)") {
      names(ctempfilter)[1] <- "1"
    }
  
    #build 
    model = as.formula(paste(node, "~", paste(names(ctempfilter), collapse = "+")))
    
    
    #model1 <- egbn.coefs2formula(ctempfilter)
    #fittedformula <- as.formula(model1)
    
    # this allows us to compare BIC 
    result <- -BIC(lm(model, data = workdata)) / 2.0    
    
  }
}


#Creates an extended BN with random formulas per node
egbn.addmodels = function(net, p_pwr, p_int){
  
  if (class(net) != "bn") {
    stop("'net' should be of class 'bn' ")
  }
  # vector of strings with the nodes in order..
  nodes <- node.ordering(net)
  
  # loop through the nodes
  for(node in nodes) {
    
    # get node object
    cn <- net$nodes[[node]]
    
    # Cache the parentCount, we need it multiple times.
    parentCount <- length(cn$parents)
    
    # With k parents we need at least intercept + k coefs.
    coefs = egbn.randomcoefs(1+parentCount)
    
    # If one of the parents have an interaction or power term already
    # reduce the coefs , to limit the numerical growth.
    
    
    # The intercept column name is set to "1", 
    # which makes it easier to generate a formula 
    # based on a named vector of coefficients.
    coefnames = c(c("1"),c(cn$parents))
    names(coefs) <- coefnames
    
    # remember if we need to scale down the sampled coefficients
    scaledown = FALSE;
    
    # With p_pwr chance, we add an power term of one the parents.
    p_pwr_sample =runif(1)
    if (parentCount>0 & p_pwr_sample<p_pwr) {
      
      # Pick one of the parents as a power term.
      pwr_node <- sample(cn$parents,1)
      
      # Add this node to the list of named coef
      coefs = append(coefs, egbn.randomcoefs(1))
      names(coefs)[length(coefs)] <- paste(pwr_node,"^2",sep="")
      
      # Also store the name of the power term inside the node for easy reference
      cn$powernode = pwr_node
      
      scaledown = TRUE;
    }
    
    p_int_sample = runif(1)
    if (parentCount>1 & p_int_sample<p_int) {
      
      # Pick two unique parents as an interaction term.
      int_nodes <- sample(cn$parents, size=2, replace =F)
      
      # Add this term to the list of named coef
      coefs = append(coefs, egbn.randomcoefs(1) ) 
      names(coefs)[length(coefs)] <- paste(int_nodes,collapse = "*")
      
      # Also store the names of the interaction terms inside the node.
      cn$interactionnodes = int_nodes
      
      scaledown = TRUE;
    }
    
    #scale down coefs when there interaction or power terms.
    if (scaledown) {
      coefs = coefs/10.0
    }
    
    #store the coefs and the model (formula of the mean), into the node 
    cn$coefs <- coefs
    cn$model <- egbn.coefs2formula(coefs)
    
    #store the manipulated node back 
    net$nodes[[node]] <- cn
  }
  
  #return the extend GBN, a BN with node models 
  result <- net
}

#Converts a named vector of coefficients to an expression which can be evaluated.
egbn.coefs2formula = function(named_coefs){
  terms <- paste(named_coefs,names(named_coefs) , sep="*")
  #first term is always : Interceptvalue * 1 ; 
  #for clarity , we remove the *1
  if (names(named_coefs)[1]==1) {
    terms[1] <- substr(terms[1],1,nchar(terms[1])-2)
  }
  #return the model/formula of the terms.
  result <- paste(terms, sep="", collapse=" + ")
}


#reports the number of parameters used to fit the function
#which is the Intercept + the number of terms (normal+interactions+power) if any.
#it lacks the +1 used in bic-g calculation to account for the noise/error term.
egbn.getk = function(egbn) {
  
  if (class(egbn) != "bn") {
    stop("'egbn' should be of class 'bn' ")
  }
  
  if (is.null(egbn$nodes[[1]]$fittedvalues)) {
    stop(" The provided BN is not fitted with eGBN tools.")
  }
  nodes <- node.ordering(egbn)
  sum = 0
  for (node in nodes) {
    cn <- egbn$nodes[[node]]
    sum = sum + cn$fittedk
  }
  result <- sum
  
}


#Get the free nodes of the egbn
#Free nod defined as :no parents  and no children, so unattached node
egbn.getfreenodes = function(egbn) {
  
  tmp <-unlist(lapply(egbn$nodes, function (n) ((length(n$parents)==0) & (length(n$children)==0))))
  result <- names(tmp[tmp==TRUE])
}


egbn.score = function(egbn, data, method = "loglik", debug= FALSE){
  
  if (class(egbn) != "bn") {
    stop("'egbn' should be of class 'bn' ")
  }
  
  if (is.null(egbn$nodes[[1]]$fittedvalues)) {
    stop(" The provided BN is not fitted with eGBN tools.")
  }
  
  #store fitted values
  nodes <- node.ordering(egbn)
  
  sum = 0.0 
  for (node in nodes) {
    cn <- egbn$nodes[[node]]
    cn$name = node
    sum = sum + egbn.score.per.node(cn,data,method=method, debug)
  }
  result <- sum
}

# add score to each node in the eGBN
egbn.addscore = function(egbn, data, method = "loglik", debug= FALSE){
  nodes <- node.ordering(egbn)
  for (node in nodes) {
    cn <- egbn$nodes[[node]]
    cn$name = node
    cn$score = egbn.score.per.node(cn,data,method = method, debug)
    egbn$nodes[[node]] <- cn
  }
  result <- egbn
}

# custom implementation of scores  
egbn.score.per.node = function(node, data, method="loglik", debug = FALSE){
  
  #assumes the node has
  # fittedformula 
  # fittedvalues   
  # a name property 
  #residuals 
  
  #TODO : on the fly generation of fittedvalues?
  node$residuals <- data[,node$name] - node$fittedvalues
  
  node$sd <- sd(node$residuals)
  
  loglik = 
    sum(dnorm(data[,node$name], 
              mean = node$fittedvalues, 
              sd=node$sd, 
              log= TRUE))
  
  if (debug){
    print( paste(node$name,"has loglik", loglik))
  }
  
  if (method=="bic-g"){
    #add the extra 'penalty' 
    penalty = (node$fittedk+1) * log(length(data[,1])) * 0.5
    if (debug){
      print(paste(node$name,"has penalty-term: ", penalty))
    }
    result <- loglik - penalty
  } else
  {
    result <- loglik  
  }
  
 
}

# add formulas to the nodes.
egbn.fit = function(egbn, data, method = "lm" , augment = FALSE)
{
  #check parameters
  if (class(egbn) != "bn") {
    stop("'egbn' should be of class 'bn' ")
  }
  if (class(data) != "data.frame") {
    stop("'data' should be of class 'data.frame' ")
  }

  #zoek per node een model (volgens method) wat de data fit.
  nodes <- node.ordering(egbn)
  
  for (node in nodes) {
    # get node object
    cn <- egbn$nodes[[node]]
    cn$name <- node
    
    # fit for deze node. 
    cn <- egbn.fit.per.node(cn, data, method, augment)
    
    # sla node weer terug op
    egbn$nodes[[node]] <- cn
  }
  #return fitted egbn
  #return the extend GBN, a BN with node models 
  egbn$fitted = TRUE
  egbn$fitted_method = method
  egbn$fitted_augmented = augment
  result <- egbn
}

egbn.fit.per.node = function(cn, data, method = "lm", augment = FALSE){
  
  pc = length(cn$parents)
  # select data fragment 
  workdata = data[c(cn$parents)]
  
  # extend it , if augmentation is needed
  if (augment & pc>0) {
    workdata <- egbn.augmentdata(workdata)
  }
  
  #columns of the data (parents + augmented)
  dcols = colnames(workdata)
  
  # put the original data back in workdata
  workdata[cn$name] = data[cn$name]
  
  # glm is only usefull when parents are at least 0 or 1 
  # parentcount=0 
  if (pc==0) {method="lm"}
  if (pc==1 & augment==FALSE) {method="lm"}
  
  if (method == "lm") {
    # bepaal het linear model wat deze node zou kunnen zijn
    # adhc aantal dcols
    # Ex. dcols are A and B
    # Ex. Coefnames= 1,A,B
    
    coefnames = c(c("1"),colnames(workdata[dcols]))
    # Ex. Terms = 1+A+B
    terms <- paste(coefnames,collapse = "+")
    # Ex. Linearmodel description = " C ~ 1+A+B " 
    lmdesc <- paste(cn$name," ~ ", terms)
    # voer een linear model voor deze node uit
    model <- lm(lmdesc, workdata)
    # maak een complete formule ?
    # Ex. modelcoefs = Intercept , A, B
    modelcoefs <- coefficients(model)
    # sometimes LM gives back NA's in their coef. 
    # remove them.
    modelcoefs <- modelcoefs[!is.na(modelcoefs)]
    # Change Intercept to 1
    if (names(modelcoefs)[1] == "(Intercept)") {
      names(modelcoefs)[1] <- "1"
    }
    # Make a complete formula based on the named coefficients
    # Ex. FittedFormula = 2.1  + 1.0*A + 1.1*B
    fittedformula <- egbn.coefs2formula(modelcoefs)
     #store fitted values 
    cn$fittedformula <- fittedformula
    cn$fittedvalues <- eval(parse(text=fittedformula),envir=workdata)
    cn$fittedk <- length(modelcoefs)
    
  } else if (method=="glm")
  {
    # in this model , we use LASSO 
    # zoek naar de lamba
    cv_model <- cv.glmnet(data.matrix(workdata[dcols]), 
                          data.matrix(workdata[cn$name]), alpha = 1)
    
    #find optimal lambda value that minimizes test MSE
    best_lambda <- cv_model$lambda.min
    
    best_model <- glmnet(data.matrix(workdata[dcols]), 
                         data.matrix(workdata[cn$name]), alpha = 1, 
                         lambda = best_lambda)
    
    #we have a model.. filter the coefs.
    ctemp <- coefficients(best_model)[,1]
    ctempfilter <- ctemp[ctemp!=0]
    #replace the term Intercept, if needed. (sometimes there is not intercept)
    if (names(ctempfilter)[1] == "(Intercept)") {
      names(ctempfilter)[1] <- "1"
    }
    
    fittedformula <- egbn.coefs2formula(ctempfilter)
    
    cn$fittedformula <- fittedformula
    cn$fittedvalues <- eval(parse(text=fittedformula),envir=workdata)
    cn$fittedk <- length(ctempfilter)
    
    
  } else {
    stop(paste(method," is not an implemented method."))
  }
  
  result <- cn
}



#Draw samples from the eGBN as a dataframe
egbn.sample = function(egbn ,count, samplesd) 
{
  nc <-length(egbn$nodes)
  
  # create empty dataframe of the correct size
  df <- data.frame( matrix(data=NA, 
                           nrow=count, 
                           ncol=nc, 
                           dimnames =list(NULL, names(egbn$nodes))))
  
  # fill each column with samples from N(mean, sd)
  # where mean is calculated based on the model attached to each node.
  
  # we need the correct ordering  
  nodes <- node.ordering(egbn)
  
  for(node in nodes) {
    cn <- egbn$nodes[[node]]
    # parse the expression of the model associated with this node
    
    f  <- parse(text=cn$model)
    # fill the column of the dataframe, based on the model f.

    df[, node] <- rnorm(count, eval(f,envir=df),samplesd)    
  
    
    #means <- eval(f,envir=df)
    #if (length(means)==1){
    #  df[, node] <- rnorm(count, eval(f,envir=df),samplesd)
    #} else
    #{
    #  localsd = sd(means)
    #  df[, node] <- rnorm(count, eval(f,envir=df),localsd)
    #}

   
  }
  result <- df
}


egbn.printmodels = function(egbn)
{
  models <-lapply(egbn$nodes, function (n) n$model)
  for (n in names(models))
  {
    cat(paste(n, " = ", models[n], "\n") )
  }
}

#returns the number of interactions found in the models of the eGBN
egbn.totalinteractions = function(egbn)
{
  tmp <-lapply(egbn$nodes, function (n) if ( is.null(n[["interactionnodes"]])) { c(0) } else { c(1)})
  result <- sum(unlist(tmp))
}

#returns the number of power terms found in the models of the eGBN
egbn.totalpowers = function(egbn)
{
  tmp <-lapply(egbn$nodes, function (n) if ( is.null(n[["powernode"]])) { c(0) } else { c(1)})
  result <- sum(unlist(tmp))
}

egbn.getnodeswithinteraction = function (egbn){
  
  if (length(egbn$fitted)==1) {
    #for each node, get the formula and extract interaction nodes..
    result<-unique(unlist(lapply(egbn$nodes, function(n) egbn.extractinteractiontermsformula(n))))
  } else
  {
    tmp = lapply(egbn$nodes, function (n) if ( !is.null(n[["interactionnodes"]])) { n[["interactionnodes"]] })
    names(tmp) <- NULL
    result <- unlist(tmp)
  }
}

egbn.getnodeswithpower = function (egbn){
  if (length(egbn$fitted)==1) {
    result<-unique(unlist(lapply(egbn$nodes, function(n) egbn.extractpowertermsformula(n))))
    
  }else{
   tmp = lapply(egbn$nodes, function (n) if ( !is.null(n[["powernode"]])) { n[["powernode"]] })
   names(tmp) <- NULL
   result <- unlist(tmp)
  }
}

#extracts the node names in a formula which represents powerterms.
#for example, a formula : 1+ 2*x4 + 3*x2 + 4*x1x1 + 2*x1x4 
#will return x1 as the node name used as power term.
egbn.extractpowertermsformula = function (modelstring ){
  
  #remove all space of formula
  f <-gsub(" ", "", modelstring, fixed = TRUE)
  
  #split all + and * parts
  tmp <-unlist(strsplit(unlist(strsplit(f,"+",fixed=TRUE)),"*",fixed=TRUE))
  
  #only use the parts starting with an x
  tmp2 <- tmp[startsWith(tmp,"x")]

  #only use the parts that have 2 x-s..
  tmp3<-tmp2[lapply(strsplit(tmp2,"x",fixed=TRUE), function(n) length(n)) ==3]

  #power terms
  tmp4 <- unlist(lapply(strsplit(tmp3,"x",fixed=TRUE) , function (n) if (n[2]==n[3]) {n[2]}))
  
  #restore the x in front of this list
  result <- unlist(lapply(tmp4, function(n) paste0("x",n)))
}

#extracts the node names in a formula which represents interactions
#for example, a formula : 1+ 2*x4 + 3*x2 + 4*x1x1 + 2*x1x4 
#will return x1 and x4 as node names used in an interaction term.
egbn.extractinteractiontermsformula = function (formulastring ){
  
  #remove all space of formula
  f <-gsub(" ", "", formulastring, fixed = TRUE)
  
  #split all + and * parts
  tmp <-unlist(strsplit(unlist(strsplit(f,"+",fixed=TRUE)),"*",fixed=TRUE))
  
  #only use the parts starting with an x
  tmp2 <- tmp[startsWith(tmp,"x")]
  
  #only use the parts that have 2 x-s..
  tmp3<-tmp2[lapply(strsplit(tmp2,"x",fixed=TRUE), function(n) length(n)) ==3]
  
  #power terms
  tmp4 <- unlist(lapply(strsplit(tmp3,"x",fixed=TRUE) , function (n) if (n[2]!=n[3]) {c(n[2],n[3])}))
  
  #restore the x in front of this list
  result <- unlist(lapply(tmp4, function(n) paste0("x",n)))
}



# extend the dataset with power- and interaction terms
egbn.augmentdata = function(data){
  
  # add power and interaction terms
  cns <- colnames(data)
  
  if (length(cns)==0){
    return(data)
  }
  
  if (length(cns)>100){
    stop("augmenting data frame with more then 100 variables is not feasable.")
  }
  
  
  for (i in 1:length(cns))
  {
    for (i2 in i:length(cns))
    {
      newcn = paste(cns[i],cns[i2], sep="")
      data[,newcn] = data[cns[i]]*data[cns[i2]]
    }
  }
  result <- data
}


# simple utility function 
egbn.log = function(arg){
  print(arg)
}




