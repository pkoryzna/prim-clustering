# prim-clustering - A very simple Prim's algorithm implementation with Akka Clustering
This is a simple implementation of parallelized Prim's minimum spanning tree search
algorithm, using Akka Clustering to distribute work between workers, based on the 
[Akka Cluster Samples with Scala](http://www.typesafe.com/activator/template/akka-sample-cluster-scala)
Activator template.

## Algorithm
The algorithm used is very similar to regular Prim, except that the step in which
the closest reachable node is found is executed in parallel, by the worker actors: 
the adjacency matrix is split in one dimension into _n_ parts, with _n_ being number of
registred workers.

## How to run

### Starting the frontend
Run `sbt 'runMain rsi.graph.distributed.actors.PrimFrontend 2552'`. 
This will start up the frontend and start sending jobs with varying number of workers enabled.

### Spawning workers
Use `sbt 'runMain rsi.graph.distributed.actors.BackendWorker 2551'` for the first instance of worker, then 
`sbt 'runMain rsi.graph.distributed.actors.BackendWorker 0'` for the rest. The workers and frontend are ran in
separate ActorSystems.

## Possible improvements
  - reading graphs from file, instead of hardcoded examples
  - simpler way of starting up
  - improved configuration 
