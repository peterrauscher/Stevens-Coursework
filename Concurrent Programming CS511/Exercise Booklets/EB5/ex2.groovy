import java.util.concurrent.Semaphore

N = 40 // number of feeding lots
Semaphore feedLot = new Semaphore(N)
Semaphore feedMutex = new Semaphore(1)
Semaphore catMutex = new Semaphore(1)
Semaphore dogMutex = new Semaphore(1)
cats = 0
dogs = 0

20.times { // Cat
    Thread.start {
        catMutex.acquire()
        cats++
        if (cats == 1) {
            feedMutex.acquire()
        }
        catMutex.release()
        feedLot.acquire() // access feeding lot
        println("A cat has eaten") // eat
        feedLot.release() // leave feeding lot
        catMutex.acquire()
        cats--
        if (cats == 0) {
            feedMutex.release()
        }
        catMutex.release()
    }
}

20.times {
    Thread.start { // Dog
        dogMutex.acquire()
        dogs++
        if (dogs == 1) {
            feedMutex.acquire()
        }
        dogMutex.release()
        feedLot.acquire() // access feeding lot
        println("A dog has eaten") // eat
        feedLot.release() // leave feeding lot
        dogMutex.acquire()
        dogs--
        if (dogs == 0) {
            feedMutex.release()
        }
        dogMutex.release()
    }
}