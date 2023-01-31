# Introduction

## Process Interaction

- Interacting processes share **resources**

## Concurrency

- The study of systems of interacting programs which share resources and run _logically_ at the same time
  - **Parallelism** involves programs running physically at the same time, but this course ignores the distinction
- Two models of interaction
  1. Shared memory
  2. Message passing

## Competitive Processes

- **Deadlock:** a set of processes are blocked because each process is holding a resource and waiting for another resource acquired by some other process.
  ![](https://i.ibb.co/ZhRJ18d/deadlock.png "Deadlock illustrated")
- **Livelock:** two or more processes continually repeat the same interaction in response to changes in the other processes without doing any useful work.
- **Starvation:** a process is perpetually denied necessary resources, so the program never progresses.

## Modelling Program Execution

```groovy
P = Thread.start{
  print "A";
  print "B";
}
Q = Thread.start{
  print "C";
  print "D";
}
P.join()
Q.join()
```

Possible output:

1. ABCD
2. ACDB
3. ACBD
4. CABD
5. CDAB
6. CADB

- If P has m insturctions and Q has n instructions, then there are $\frac{(m+n)!}{m!n!}$ interleavings

### Transition Systems

A transition system is a tuple of the form $(S, \rightarrow, I)$ where

- S is a set of states
- $\rightarrow \subseteq S \times S$ is a transition relation
- $I \subseteq S$ is the set of initial states

### Example of a Transition System

![](https://i.ibb.co/mHvHb9W/Screenshot-2023-01-21-at-10-55-50-AM.png "Transition system")

- Examples of paths in textual notation:
  - $(P4, Q8, 0) \rightarrow (-, Q8, 1) \rightarrow (-, -, 2)$
  - $(P4, Q8, 0) \rightarrow (P4, -, 2) \rightarrow (-, -, 1)$
  - $(P4, Q8, 0) \rightarrow (P4, -, 2)$
