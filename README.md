# egbn
Extended Gaussian Bayesian Network


# Distributed setup

Some python scripts which use Azure Storage to store 'tasks' which are runned by taskrunner.py.

A Docker containing both Python and R.

# Downloading the results.

Install AzCopy
https://docs.microsoft.com/nl-nl/azure/storage/common/storage-use-azcopy-v10


download the results from azure storage to rawresults folder.

.\bin\azcopy.exe copy "https://egbntasks.file.core.windows.net/queue1/results/*?sv=2021-06-08&ss=bfqt&srt=sco&sp=rwdlacupiytfx&se=2024-01-04T16:21:23Z&st=2023-01-04T08:21:23Z&spr=https&sig=DQdl%2Ft6KumdzLwTNuX%2FlzrrP%2FEfRZPj6ik11gl%2Fiuv4%3D" rawresults



# Parsing the results

.res files are the STDOUT of the Rscript processes.
it contains a marker "###" that indicates the start of the JSON output. 

python ./parser/parse.py


# Docker notes

docker build -t egbn:v4 -f docker/Dockerfile .
docker run --env SASKEY="?sv=2021-06-08&ss=bfqt&srt=sco&sp=rwdlacupitfx&se=2023-06-24T04:59:59Z&st=2022-06-23T20:59:59Z&spr=https&sig=iVu9UxCTdUc1LO9n%2FNuVu4Ueq9jWEk2ThAJvlypwxxQ%3D" -it egbn:v4


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


de0a3b3c-89da-452f-bd70-199749ccb763

= VS- Lucas
az account set --subscription cf72eb5f-a531-4263-8445-ee53f2f130ad 


az container create -g rg-egbn --name egbnrunner2 --image egbnreg.azurecr.io/egbn:v4 --environment-variables SASKEY="?sv=2021-06-08&ss=bfqt&srt=sco&sp=rwdlacupitfx&se=2023-06-24T04:59:59Z&st=2022-06-23T20:59:59Z&spr=https&sig=iVu9UxCTdUc1LO9n%2FNuVu4Ueq9jWEk2ThAJvlypwxxQ%3D" --registry-username egbnreg --registry-password iQHF+zLH994qDkUD1XFwPjvDQebobyqk --os-type Linux --ip-address Public --command-line "python taskrunner.py"

# 
