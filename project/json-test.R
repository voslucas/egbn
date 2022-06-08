# experiments with JSON in R
library("rjson")

output <- list()
output$test = 1.3

js <- toJSON(output,indent = 2)
write(js,file="output.json")
