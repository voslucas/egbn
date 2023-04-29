# script to manually implement loglikelihood 
# we compare it with the bnlearn builtin loglikehood to make sure we get similar results
library(bnlearn)

data <- score(bn, data=gaussian.test, type="loglik-g", debug= TRUE)
gaussian.test
bn <- model2network("[A][B][E][G][C|A:B][D|B][F|A:D:E:G]")

fitted <- bn.fit(bn, data)

# toon de ingebouwde loglikelihoods per node (debug)



# voor een willekeurige node doe je :
# - bepaal een model = fit de parameters van de parents
# - bepaal hoe het model zou reageren op de parents
# - bekijken de afwijking tussne data en het met-parents-gevulde model
# - bepaald 

nodes <- node.ordering(bn)



m_A <- sum(gaussian.test$A)/length(data$A)
s_A <- sd(data$A)

# handmatig het rijtje langslopen.. van node A
sum(dnorm(gaussian.test$A,mean =m_A, sd = s_A , log = TRUE))

# dit geeft idd dezelfde loglik als score functie.. 
# maar nu node C.
# node C is afhankelijk van A en B
# de gevonde n'formule' is ongeveer C = 2+ 2A + 2B

m_C <- sum(gaussian.test$C)/length(data$C)
s_C <- sd(2.001+1.996*gaussian.test$A + 1.999*gaussian.test$B)

sd(fitted$C$residuals)

sum(dnorm(gaussian.test$C, mean = 2.001+1.996*gaussian.test$A + 1.999*gaussian.test$B, sd=0.5, log= TRUE))

test <- gaussian.test$C - (2.001+1.996*gaussian.test$A + 1.999*gaussian.test$B)
test <- test*test
sqrt(sum(test)/5000)
test <- test*5000
sd(test)

