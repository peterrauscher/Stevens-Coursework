// one-time use (non-cyclic) barrier
import java.util.concurrent.Semaphore

final int N = 5
Semaphore barrier = new Semaphore(0)
Semaphore mutex = new Semaphore(1)
int counter = 0

N.times {
    Thread.start {
        
        mutex.acquire()
        counter++
        if(counter == N) {
            N.times { barrier.release() }
        }
        mutex.release()

        barrier.acquire();
    }
}