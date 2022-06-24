import os
import json
from pandas import DataFrame
import pandas as pd


#For the excel export 
#pip install -r requirements.txt

def getSDfromFileName(fname):
    #example fname is 'rawresults/task-1-10-50-0.1-0.5-7-1.0.res'
    #remove the .res
    p1= fname[:-4]
    #split on -
    p2 = p1.split("-")
    # the last value is the SD
    return float(p2[-1])

def convert(fname):
    f = open(fname,"r")
    content = f.read()
    #we left a marker in the file +++ where we can split normal output from the json output.
    tmps = content.split("\"+++\"")
    #skip 5 characters, because the string starts with "[1] .. "  as normal with Rscript output.  
    json_str = tmps[1][5:]
    #het parsen moet 2x .. want 1e keer stript ie alleen de "" eraf.. 
    data = json.loads(json_str)
    data2 = json.loads(data)

    #breng alle lists die in data2 zitten , terug naar een simpele string
    listCols = [i for i in data2.keys() if isinstance(data2[i], list)]
    for listCol in listCols:
        data2[listCol] = ";".join(data2[listCol])

    #we zijn vergeten om de gebruikte SD ook in de JSON output op te slaan, maar gelukkig zit die wel in de filename.
    mysd = getSDfromFileName(fname)
    data2["sd"] = mysd

    return data2


dictCollection = []

#lees alle res bestandjes in , en zet ze om in dictionary's
for file in os.listdir("rawresults"):
    if file.endswith(".res"):
        df= convert("rawresults/"+file)
        dictCollection.append(df)


#maak een dataframe van onze dictionaries
dffinal = DataFrame(dictCollection)

dffinal.to_pickle("results.pickle")
dffinal.to_excel("result.xls")


print(dffinal)
print("hello")
