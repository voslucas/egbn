# Generate tasks for the task manager.
import os
import subprocess
import datetime

from task-manager import TaskManager 
taskManager = TaskManager("queue1")


nodecounts = [10,100,1000]
datasizes = [50,500,5000]


for nodecount in nodecounts:  
    for datasize in datasizes:
        name = "task-{}-{}".format(nodecount,datasize)
        fname = name + ".tsk"
        #prevent recreating the task ,if it is already there..
        if os.path.exists(fname)==False:
            command = "RSCRIPT method.py --nodecount {} --datasize {}".format(nodecount,datasize)
            print("Adding task with name {} at {} by executing {}.".format(name,datetime.datetime.now(),command))
            taskManager.addTask(name,command)
