
# network model was generated from melancon...
dag = model2network("[x3][x2|x3][x4|x2][x8|x3:x4][x1|x4:x8][x5|x1:x3][x6|x1:x4:x8][x7|x1:x5:x8][x9|x5:x6][x10|x6:x9]")

hcexperiment = function(c,my_sd)
{
  # add distributions
  distx1 =list(coef = c("(Intercept)" = 1, "x4" = c, "x8" = c), sd = my_sd)
  distx2 =list(coef = c("(Intercept)" = 1, "x3" = c)          , sd = my_sd)
  distx3 =list(coef = c("(Intercept)" = 1)                    , sd = my_sd)
  distx4 =list(coef = c("(Intercept)" = 1, "x2" = c)          , sd = my_sd)
  distx5 =list(coef = c("(Intercept)" = 1, "x1" = c, "x3" = c), sd = my_sd)
  distx6 =list(coef = c("(Intercept)" = 1, "x1" = c, "x4" = c, "x8" = c), sd = my_sd)
  distx7 =list(coef = c("(Intercept)" = 1, "x1" = c, "x5" = c, "x8" = c), sd = my_sd)
  distx8 =list(coef = c("(Intercept)" = 1, "x3" = c, "x4" = c), sd = my_sd)
  distx9 =list(coef = c("(Intercept)" = 1, "x5" = c, "x6" = c), sd = my_sd)
  distx10=list(coef = c("(Intercept)" = 1, "x6" = c, "x9" = c), sd = my_sd)
  
  #custom fitted bn
  fitted = custom.fit(dag, dist= list(x1=distx1,x2=distx2,x3=distx3,x4=distx4,x5=distx5,x6=distx6,x7=distx7,x8=distx8,x9=distx9,x10=distx10))
  #draw samples
  dataset= rbn(fitted, n = 1000)
  #try to recover with scorebased hillclimb
  recovered = hc(dataset) #, score="bic-g")
  #record the hammingdistance
  hamdist = hamming(recovered,dag)
  result<-hamdist
}

avg_hamdist1 <- replicate(500, hcexperiment(1.0, 0.5)) 
print(summary(avg_hamdist1))

avg_hamdist1 <- replicate(500, hcexperiment(10.0,0.5)) 
print(summary(avg_hamdist1))

avg_hamdist1 <- replicate(500, hcexperiment(0.1, 0.5)) 
print(summary(avg_hamdist1))



