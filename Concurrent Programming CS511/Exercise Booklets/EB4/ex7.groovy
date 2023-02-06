import java.util.concurrent.Semaphore

Semaphore okA = new Semaphore(1)
Semaphore okB = new Semaphore(1)

Thread.start {
    while (true) {
        okA.acquire()
        print "A"
        okB.release()
    }
}

Thread.start {
    while (true) {
        okB.acquire()
        print "B"
        okA.release()
    }
}

// 1. Use semaphores to guaranteee that at all times the number
// of A’s and B’s differs at most by 1.
// A: The above code guarantees this.

// 2. Modify the solution so that the only possible output is ABABABABAB...
// A: Simply change the initial permits of okB above to 0.