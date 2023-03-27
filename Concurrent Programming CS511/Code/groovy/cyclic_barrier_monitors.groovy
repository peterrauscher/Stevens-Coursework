import java.util.concurrent.locks.*

class Barrier {
    int size
    int numReached = 0
    Lock lock = new ReentrantLock()
    Condition waitingAtBarrier = lock.newCondition()
    
    public Barrier(int size) {
        this.size = size
    }

    public void reached_barrier() {
        lock.lock()
        try {
            numReached++
            println("numReached: " + numReached)
            if (numReached == size) {
                numReached = 0
                waitingAtBarrier.signalAll()
            }
            while (numReached < size) {
                waitingAtBarrier.await()
            }
            waitingAtBarrier.signalAll()
        } finally {
            lock.unlock()
        }
    }
}

Barrier b = new Barrier(3)

Thread.start {
    while (true) {
        print "A"
        b.reached_barrier()
        print "1"
    }
}

Thread.start {
    while (true) {
        print "B"
        b.reached_barrier()
        print "2"
    }
}

Thread.start {
    while (true) {
        print "C"
        b.reached_barrier()
        print "3"
    }
}