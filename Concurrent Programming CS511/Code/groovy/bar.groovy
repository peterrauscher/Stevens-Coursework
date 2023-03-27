import java.util.concurrent.Semaphore

Semaphore mutex = new Semaphore(1)
int ratio = 0
int jets = 0
int patriots = 0

100.times { // Patriots
    Thread.start {
        mutex.acquire()
        patriots++
        mutex.release()
    }
}

100.times { // Jets
    Thread.start {
        mutex.acquire()
        jets++
        mutex.release()
    }
}