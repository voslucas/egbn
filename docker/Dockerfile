FROM python:3.8-slim
RUN apt-get update && \
    apt-get install -y --no-install-recommends r-base r-base-dev && \ 
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY project/requirements.r .
COPY distributed/requirements.txt .

COPY distributed/taskmanager.py .
COPY distributed/taskrunner.py .

COPY project/egbn-lib.R .
COPY project/datacollector.R .

RUN pip3 install -r requirements.txt

RUN Rscript requirements.r

CMD ["python","taskrunner.py"]
