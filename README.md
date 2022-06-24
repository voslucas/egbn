# egbn
Extended Gaussian Bayesian Network



# Distributed setup

Some python scripts which use Azure Storage to store 'tasks' which are runned by taskrunner.py.

A Docker containing both Python and R.

# Downloading the results.

Install AzCopy
https://docs.microsoft.com/nl-nl/azure/storage/common/storage-use-azcopy-v10


download the results from azure storage to rawresults folder.

.\bin\azcopy.exe copy "https://egbntasks.file.core.windows.net/queue1/results/*?sv=2021-06-08&ss=bfqt&srt=sco&sp=rwdlacupitfx&se=2023-06-24T04:59:59Z&st=2022-06-23T20:59:59Z&spr=https&sig=iVu9UxCTdUc1LO9n%2FNuVu4Ueq9jWEk2ThAJvlypwxxQ%3D" rawresults

# Parsing the results

.res files are the STDOUT of the Rscript processes.
it contains a marker "###" that indicates the start of the JSON output. 

python ./parser/parse.py


# Docker notes

docker build -t egbn:latest -f docker/Dockerfile .
docker run --env SASKEY="?sv-...." -it egbn:latest


connection to azure container registery

Tag the image

docker tag test:latest egbnreg.azurecr.io/egbn:v1

docker login egbnreg.azurecr.io

docker push egbnreg.azurecr.io/egbn:v1

# Azure notes

You will need
- blob storage account , with a SAS key.
- container registrary
- container instances

az login
az account set --subscription de0a3b3c-89da-452f-bd70-199749ccb763

az container create -g egbn --name egbnrunner4 --image egbnreg.azurecr.io/egbn:v3 --environment-variables SASKEY="?sv=2021-06-08&ss=bfqt&srt=sco&sp=rwdlacupitfx&se=2023-06-24T04:59:59Z&st=2022-06-23T20:59:59Z&spr=https&sig=iVu9UxCTdUc1LO9n%2FNuVu4Ueq9jWEk2ThAJvlypwxxQ%3D" --registry-username egbnreg --registry-password iQHF+zLH994qDkUD1XFwPjvDQebobyqk --os-type Linux --ip-address Public --command-line "python taskrunner.py"

# 
