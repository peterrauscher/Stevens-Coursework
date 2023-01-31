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

A common abbreviation for doing nothing until a condition holds is `await !cond` which is logically equivalent to`while (cond) {}`
