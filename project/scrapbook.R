
# scrapbook 

# enormous amount of try outs, discovery . etc..


dag = random.graph(cnames, 
                   num = 1, # For now, we only need one network ... 
                   method = "melancon", 
                   max.in.degree=max_degree, 
                   max.out.degree=max_degree )


test <- list( 
          x1= list(a=1,b=2),
          x2= list(a=3,b=4))

test2<-lapply(test, function(x) x[["a"]])


myegbn$nodes$x5$powernode


test5 <- sum(unlist(test3))

sum(test3)
test4 <- sapply(test3, sum)
sum(test4)

print(net.coefs2formula(c("x1"=3,"x2"=3)))
print(net.coefs2formula(coefs))
coefs

net.generateformula = function(node) {
  print(node)
}

#


any("bn" = class(net))

net2 <- net.generateformulas(net)
class(net) <- c(class(net),"eGBN")
class(net)





df <- net.GenerateSamples(net)

nodes <- node.ordering(net)


cn2 <-  net$nodes[["x2"]]

testint_nodes <- sample(cn2$parents, size=2, replace =F)
paste(testint_nodes,collapse="*")

hasParents <- length(cn2$parents)>0
cn2["formula"] = "1"
cn2$parents

c(c("intercept"),c(cn2$parents))

# bij k parents heb ik 
# 1+k coefs nodig
parents = c("x1","x2")
k = length(cn2$parents)
coefs = sample(allowed_coefvalues,1+k)
coefnames = c(c("1"),c(cn2$parents))
names(coefs) <- coefnames

# add power term
coefs = append(coefs, sample(allowed_coefvalues,1))
coefnames = append(coefnames,)
names(coefs)[length(coefs)] <- "x4^2"

#can we build a string-formula based on the names coefs?


coefs

test <- c("a"=1,"b"=2)
test <- append(test, 3)
test2<-test["a"]
test

tmp_names <- c("intercept")

tmp_coefs <- c("intercept"= sample(allowed_coefvalues,1))
append(tmp_coefs,sample(allowed_coefvalues,3))
append(tmp_names,cn2$parents)
tmp_names
net2 <- net.generateformulas(net)

formulestr <- "2+4.0*x1data+2.0*x2data+3.0*x1data*x2data"

iets <- NULL
iets <- eval(parse(text=formulestr))
iets[1]
# add a model >

test<- c(A=1,B=2)

length(cn$parents)

# je hebt 3 parents? 
# bv x3,x4,x5
# bepaal of er een power in moet zitten
# if change>0.05.. ; kies 1 parents
# bepaal of er een interactie in moet zitten. 
# if change>   ; kies 2 parents 
# bouwe formule op 
# paste(parents,"+")
# paste()

cn["formula"] <- function(data) {
  result <- data*2
}
iets <- expression()
f <- y ~ x1 + x2^2 + x1:x2
class(f)
terms.formula(f)

eval(f, substitute(quote(x1<-2)))


g <- quote(f)
eval(g)
call(g(1))

do.call("f", list(NULL))

iets <- parse(text= " x3data^2 ")

substitute(iets, x1)

eval(iets)

x1data <- rnorm(100, 10,1)
x2data <- rnorm(100, f(1,2), 1)

f <- expression(x1data *2)

test <- list()

for(node in node.ordering(net)) {
  test[[node]] = rnorm(100, 10,1)
}

test[[node.ordering(net)]] = list(1,2,3)
test[["x1"]] = list(1,2,3)

test2 <- gaussian.test

class(test2)
class(test)

#first_column <- c("value_1", "value_2", ...)
# second_column <- c("value_1", "value_2", ...)
#df <- data.frame(first_column, second_column)

append(cn, "")

no[[1]]$test <- "hallo"



currentnode <- net$nodes[[no[1]]]

x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)

net$nodes$x1$parents

# x3 heeft parents x5 ? 
# dan wil ik een formule bij X3 ?
# die formule hangt af van p_int and p_pwr ? 


net1 = net[0]

net = model2network("[A][B][C|A:B]")
distA = lm(A ~ 1, data = gaussian.test)
distB = lm(B ~ 1, data = gaussian.test)
distC = lm(C ~ A + A*B, data = gaussian.test)

coef(distC)
cfit = custom.fit(net, dist = list(A = distA, B = distB, C = distC))

cfit




include()

# aantal rijen in onze sample dataset
rows = 1000

# sample bestaat uit 3 kolommen A,B en C .. 
# we gaan proberen C te vullen met allerlei non-linear verbanden op basis van A en/of B

samples = data.frame(A = rnorm(rows, 10, 3),
                     B = rnorm(rows, 10, 3),
                     C = rep(0,rows))

# vul C bijvoorbeeld met A^2  + wat ruis
samples$C = (samples$A * samples$A * rnorm(1,1)) + rnorm(rows, 1, 1)

# andere voorbeelden: C = A*B  + wat ruis
samples$C = (samples$A * samples$B) + rnorm(rows, 1, 1)
samples$C = (samples$A ^ 3 * samples$B) + rnorm(rows, 1, 1)
samples$C =  rnorm(rows, 10, 5)


# hill climb score based;
stru3 = NULL
stru3 <- hc(samples)
stru3
stru3$nodes
# display structures
iets = bn.fit(stru3,samples)

# replace the parameters of the node F.

iets$C = list(coef = c(1, 2), sd = 3)
# set again the original parameters
iets$C = lm(C ~ A , data = samples)
iets$C
iets = bn.fit(stru3,samples, )
iets$C = list(coef = c(1, 2,3 ,4), sd = 3)
iets$C = glm(C ~ A*A , data = samples)
iets$C

iets$C = lm(C ~ C:A , data = gaussian.test)
score(stru3, samples)
glm.fit()

#stru3 <- set.arc(stru3, from ="A" , to = "C")
#stru3

# Problem is : the structure has 'succesfully' found depency between A and C..
# Which i thought (hoped due to research question) it would NOT find , without additional help.

# QUESTION : 
# Can we define a minimal dataset / example is such a way that : 
# A The learned structure will NOT pick up the nonlinear dependency
# B But, when we a manual add the arc , it will indeed show a better score.

# I cannot find a sample set (sample C) which generates the main problem we try to solve..


stru1 <- iamb(b, test ="cor");
stru1
score(stru1, b)
score(stru1, b, type="bge")

stru2 <- iamb(b, test ="cor" ,whitelist = matrix(c("A","B"),ncol=2));
stru2
score(stru2, b , type="bic-g")
score(stru2, b , type="bge")

stru3 <- hc(b)
stru3
score(stru3, b , type="bic-g")
score(stru3, b , type="bge")


# network specification.
dag = model2network("[A][B][E][G][C|A:B][D|B][F|A:D:E:G]")

bn = custom.fit(dag, list(
  A = list(coef = c("(Intercept)" = 1), sd = 1),
  B = list(coef = c("(Intercept)" = 2), sd = 3),
  C = list(coef = c("(Intercept)" = 2, "A" = 2, B = 2), sd = 0.5),
  D = list(coef = c("(Intercept)" = 6, "B" = 1.5), sd = 0.33),
  E = list(coef = c("(Intercept)" = 3.5), sd = 2),
  F = list(coef = c("(Intercept)" = 0, "A" = 2, "D" = 1, "E" = 1, "G" = 1.5), sd = 1),
  G = list(coef = c("(Intercept)" = 5), sd = 2)
))
bn$F
A2 = list(coef = c("(Intercept)" = 1), sd = 1)

library(Rgraphviz)
library(pcalg)

set.seed(101)
myDAG <- randomDAG(n = 20, prob= 0.2, lB = 0.1, uB = 1)
## require(Rgraphviz)
plot(myDAG)

