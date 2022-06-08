# hc gives different results when using custom function?
# problem fixed -> BIC(lm(..)) should return with a minus (-) 


source("egbn-lib.R")

myscore = function(node, parents, data, args) {
  
  if (length(parents) == 0)
    model = as.formula(paste(node, "~ 1"))     
  else     
    model = as.formula(paste(node, "~", paste(parents, collapse = "+")))

  result <- - BIC(lm(model, data = data)) / 2.1
}

dag.org = hc(gaussian.test, score = "bic-g")
print(score(dag.org, gaussian.test, type = "bic-g"))

dag.new = hc(gaussian.test, score = "custom", fun = myscore)
print(score(dag.new, gaussian.test, type = "custom", fun = myscore))
print(score(dag.new, gaussian.test, type = "bic-g"))
