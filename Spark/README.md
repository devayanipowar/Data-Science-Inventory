
# Apache Spark

## MapReduce to Spark

# MapReduce

In MapReduce, system loads the data applies a map function shuffles it applies a reduce function and writes it back out to persistent storage.
Here the highest level unit of computation is a job.

# Spark

Spark has a job concept but its highest level computation is at  an application which can rum multiple jobs in sequence or in parallel.


## Apache spark with python - PySpark

When running spark on YARN, each spark executor runs as a YARN container. Spark supports two modes for running on YARN, " yarn-cluster" mode and "yarn-client" mode.

Yarn-cluster -> production jobs
Yarn-client -> interactive and debugging uses where we want to see output immediately.

## YARN's application master concept.

Application is responsible for requesting resources from the resource manager and also tell node managers to start containers on its behalf.

In yarn-cluster mode, driver runs in the application master. This means that the process is responsible for both driving the application and requesting resources from YARN and thsi process runs inside a YARN container. The client doesn't need to stick around.

[Architecture]()

But yarn-cluster mode is not suitable to using spark interactively. Spark applications that require user input like PySpark need te spark driver inside client process --> the spark application.


Key Concepts:
- Application
- Spark driver
- Spark Application master
- Spark executor
- Spark Task
