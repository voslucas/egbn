# EGBN
Extended Gaussian Bayesian Network

The code as described in the research paper :

`Structure Learning Of Linear
Gaussian Bayesian Networks With Interaction Terms` 



## Source

The main code can be found in the `\project` folder. The `egnb-lib` contains the custom scoring functions, data augmentation and LASSO variable selection procedures, along with all kinds of other convencience scripts, functions and tools.

The `datacollector.R` is the setup of a SINGLE experiment where the parameters like nodecount, datasize, (see our report), can be setup through command-line parameters.


## Distributed setup

Some python scripts we used to 'scale' our calculations by queueing all task as 'script files' in an Azure Storage location. The `taskrunner.py` is used to 'scan for jobs' and executes them. 


## Parsing the results

The .res files are the STDOUT of the Rscript processes. These files contain a marker "###", which indicates the start of the JSON output. 

usage : `python ./parser/parse.py`



