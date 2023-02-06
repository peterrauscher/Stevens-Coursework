import java.util.concurrent.Semaphore

Semaphore printF = new Semaphore(0)
Semaphore printC = new Semaphore(0)

P_1 = Thread.start {
    print('A')
    printF.release()
    print('B')
    printC.acquire()
    print('C')
}

Q_1 = Thread.start {
    print('E')
    printF.acquire()
    print('F')
    printC.release()
    print('G')
}

P_1.join()
Q_1.join()
println()
