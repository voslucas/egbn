# Generate tasks for the task manager.
import os
import subprocess
import datetime

from taskmanager import TaskManager 
taskManager = TaskManager("queue1")

# Ik heb de 1000 geskipt voor nu.
reps = range(1,50)
nodecounts = [10,100]
datasizes = [50,500,5000]
pints = [0.0,0.1,0.5]
ppwrs = [0.0,0.1,0.5]
degrees = [3,5,7]
sds = [0.1,0.5,1.0]

# reps = range(1,100)
# nodecounts = [10,100]
# datasizes = [500]
# pints = [0.1]
# ppwrs = [0.1]
# degrees = [3]
# sds = [0.5]


for rep in reps:
    for nodecount in nodecounts:  
        for datasize in datasizes:
            for pint in pints:
                for ppwr in ppwrs:
                    for degree in degrees:
                        for sd in sds:
                            name = "task-{}-{}-{}-{}-{}-{}-{}".format(rep,nodecount,datasize,pint,ppwr,degree,sd)
                            #fname = name + ".tsk"
                            #prevent recreating the task ,if it is already there..
                            #if os.path.exists(fname)==False:
                            command = "Rscript datacollector.R --nodecount {} --datasize {} --pint {} --ppwr {} --degree {} --sd {}".format(nodecount,datasize,pint,ppwr,degree,sd)
                            print("Adding task with name {} at {} by executing {}.".format(name,datetime.datetime.now(),command))
                            taskManager.addTask(name,command)
