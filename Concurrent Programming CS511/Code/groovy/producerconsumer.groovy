import java.util.concurrent.Semaphore
N = 10
Integer[] buffer = new Integer[N]
int start, end
Semaphore produce = new Semaphore(N)
Semaphore consume = new Semaphore(0)
Semaphore mutexP = new Semaphore(1)
Semaphore mutexC = new Semaphore(1)
10.times {
    Thread.start {
        while (true) {
            produce.acquire()
            // produce
            mutexP.acquire()
            buffer[end] = end
            end=(end+1) % N
            mutexP.release()
            consume.release()
        }
    }
}

20.times {
    Thread.start {
        Integer item
        while (true) {
            consume.acquire()
            // consume
            mutexC.acquire()
            item = buffer[start]
            start=(start+1) % N
            print(item)
            mutexC.release()
            produce.release()
        }
    }
}