# Simple task runner. 
# Checks the queue for tasks, executes them and stores the console output back to Azure Storage.

from taskmanager import TaskManager 

taskManager = TaskManager("queue1")
taskManager.monitor()


