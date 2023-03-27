import java.util.concurrent.locks.*

class Barrier {
    int size
    int numReached = 0
    
    public Barrier(int size) {
        this.size = size
    }

    public synchronized void reached_barrier() {
        numReached++
        if (numReached < size) {
            wait()
        }
        notifyAll()
    }

}

Barrier b = new Barrier(3)

Thread.start {
        print "A"
        b.reached_barrier()
        print "1"
}

Thread.start {
        print "B"
        b.reached_barrier()
        print "2"
}

Thread.start {
        print "C"
        b.reached_barrier()
        print "3"
}