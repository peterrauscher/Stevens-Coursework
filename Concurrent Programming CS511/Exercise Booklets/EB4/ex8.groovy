import java.util.concurrent.Semaphore

n = 50
n2 = 0

Semaphore addOdd = new Semaphore(0)
Semaphore computeDone = new Semaphore(0)

P = Thread.start {
    while (n > 0) {
        addOdd.release()
        computeDone.acquire()
        n = n - 1
    }
}

Q = Thread.start {
    while (true) {
        addOdd.acquire()
        n2 = n2 + 2 * n + 1
        computeDone.release()
    }
}

P.join()
addOdd.release()
Q.stop()
print(n2)