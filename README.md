# egbn
Extended Gaussian Bayesian Network


# Distributed setup

Some python scripts which use Azure Storage to store 'tasks' which are runned by taskrunner.py.

A Docker containing both Python and R.

# Downloading the results.

Install AzCopy
https://docs.microsoft.com/nl-nl/azure/storage/common/storage-use-azcopy-v10


download the results from azure storage to rawresults folder.

.\bin\azcopy.exe copy "https://egbntasks.file.core.windows.net/queue1/results/*?sv=XXX" rawresults



# Parsing the results

.res files are the STDOUT of the Rscript processes.
it contains a marker "###" that indicates the start of the JSON output. 

python ./parser/parse.py


# Docker notes

docker build -t egbn:v4 -f docker/Dockerfile .
docker run --env SASKEY="?sv=XXX" -it egbn:v4

connection to azure container registery

Tag the image

docker tag egbn:v4 egbnreg.azurecr.io/egbn:v4

docker login egbnreg.azurecr.io

docker push egbnreg.azurecr.io/egbn:v4

# Azure notes

You will need
- blob storage account , with a SAS key.
- container registrary
- container instances

az login
