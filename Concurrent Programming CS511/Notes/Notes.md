[https://www.erlang.org/doc/system_architecture_intro/sys_arch_intro.html]()# Introduction

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

## Mutual Exclusion

### Critical Sections

**Definition:** a part of the program that access shared memory and which we wish to execute atomically

### The Mutual Exclusion Problem

MEP guarantees:

1. Mutex: At any point in time, there is at most one threa in the critical section
2. No livelock: At least one thread can enter the critical section when multiple attempt to
3. No starvation: A thread trying to enter the critical section will eventually do so

Reasonable assumptions:

- There are no variables shared between the critical section and the entry/exit protocol (The entry/exit problem cannot be logically tied to the rest of the code)
- The critical section always terminates
- The scheduler is _fair_, meaning a process waiting to run will eventually do so

A common abbreviation for doing nothing until a condition holds is `await !cond` which is logically equivalent to `while (cond) {}`

## Semaphores

### Producers / Consumers

- A pattern of interaction that accounts for differences in speed between parties, the **producers** create some data and the **consumers** use that data, once it is produced.
- Unbounded buffer

  - The producer can work freely, creating an infinite amount of data regardless of the progress of the consumer
  - The consumer must wait for the producer to produce, if it is running faster

- Bounded buffer

  - The producer can keep producing only until the buffer is full
  - The consumer must wait for the producer to produce

## Erlang

- Erlang is functional language with no shared memory. Like Python, it is dynamically typed, so the compiler will not raise type errors. Erlang is completely open source.
  `// TODO: Checkout PAXSOS distributed programming`
- Compiled code runs on a virtual machine, BEAM. Processes are lightweight which allows for high concurrency.
- Supports hotswapping (server never comes down).
- [The Open Telecom Platform (OTP)](https://www.erlang.org/doc/system_architecture_intro/sys_arch_intro.html) is a framework for developing Erlang applications that is basically a necessity to do anything interesting with it.

### Code

- In Erlang, there is no assignment. There is only matching.
  `P = E`
