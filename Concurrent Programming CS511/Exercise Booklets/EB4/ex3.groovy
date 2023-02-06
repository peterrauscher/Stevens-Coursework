import java.util.concurrent.Semaphore

Semaphore okI = new Semaphore(0)
Semaphore okO = new Semaphore(0)
Semaphore okOk = new Semaphore(0)

P_3 = Thread.start {
    print("R")
    okI.release()
    okOk.acquire()
    print("OK")
}

Q_3 = Thread.start {
    okI.acquire()
    print("I")
    okO.release()
    okOk.acquire()
    print("OK")
}

R_3 = Thread.start {
    okO.acquire()
    print("O")
    print("OK")
    okOk.release()
    okOk.release()
}

P_3.join()
Q_3.join()
R_3.join()
println()