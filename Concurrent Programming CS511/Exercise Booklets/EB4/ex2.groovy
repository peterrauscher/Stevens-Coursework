import java.util.concurrent.Semaphore

Semaphore printP = new Semaphore(0)
Semaphore printQ = new Semaphore(0)

P_2 = Thread.start {
    printP.acquire()
    print('A')
    print('S')
    printQ.release()
}

Q_2 = Thread.start {
    print('L')
    printP.release()
    printQ.acquire()
    print('E')
    print('R')
}

P_2.join()
Q_2.join()
println()