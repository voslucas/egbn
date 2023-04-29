# The "Gaussian Test" dataset is provided by the bnlearn library.
# It corresponds to the following network specification.

truedag = model2network("[A][B][E][G][C|A:B][D|B][F|A:D:E:G]")

# Below is the "script-used-to-generate" the dataset
# See https://www.bnlearn.com/documentation/networks/
# Default size value is 5000.

gendata = function(size){
  a = data.frame(A = rnorm(size, 1, 1),
                 B = rnorm(size, 2, 3),
                 C = rep(0, size),
                 D = rep(0, size),
                 E = rnorm(size, 3.5, 2),
                 F = rep(0, size),
                 G = rnorm(size, 5, 2))
  a$C = 2 * (a$A + a$B) + rnorm(size, 2, 0.5)
  a$D = 1.5 * a$B + rnorm(size, 6, 0.33)
  a$F = 2 * a$A + a$D + a$E + 1.5 * a$G + rnorm(size, 0, 1)
  result <-a
}

# Step 1. data generation. Just try different sizes.. 
trainset = gendata(5000)

# Step2 . I try to learn the dag WITH the knowledge of max 4 parents..
# which is correct , because only F has 4 parents.. 
# Documentation of hc() explains:
# maxp : 
# "the maximum number of parents for a node. The default value is Inf. "
#              ================= 

dag2 = hc(trainset, maxp = 4)

# report the hamming distance
print(hamming(truedag,dag2)) 

# Hm.. Try it few times.. You will see Hamming Distances >0 ... 
# No matter how much  data i use, it doesn't learn the true dag...

# But but but.. why? 
# As SOON as i increase the maximum number of parents of node.
dag2 = hc(trainset, maxp = 5)

# It suddenly LEARNS the true DAG.
# But this suggests that the maxp parameter of the hillclimb 
# is working differently as documented.







