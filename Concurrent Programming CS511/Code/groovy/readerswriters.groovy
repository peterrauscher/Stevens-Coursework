import java.util.concurrent.Semaphore

Semaphore resource = new Semaphore(1)
Semaphore mutex = new Semaphore(1)
Semaphore entryqueue = new Semaphore(1)

10.times {
    Thread.start { // Writer
        entryqueue.acquire()
        resource.acquire()
        entryqueue.release()
        // writes to the shared resource
        resource.release()
    }
}

20.times {
    Thread.start { // Reader
        entryqueue.acquire()
        mutex.acquire()
        r++
        if(r==1) {
            resource.acquire()
        }
        mutex.release()
        entryqueue.release()
        // reads from the shared resource
        mutex.acquire()
        r--
        if(r==0) {
            resource.release()
        }
        mutex.release()
    }
}