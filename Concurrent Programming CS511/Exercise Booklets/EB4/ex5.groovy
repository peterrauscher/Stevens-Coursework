import java.util.concurrent.Semaphore

Semaphore okF = new Semaphore(0)
Semaphore okH = new Semaphore(0)
Semaphore okC = new Semaphore(0)

N_5 = 50
P_5 = Thread.start {
    N_5.times {
        print("A")
        okF.release()
        print("B")
        okC.acquire()
        print("C")
        print("D")
    }
}
Q_5 = Thread.start {
    N_5.times {
        print("E")
        okH.release()
        okF.acquire()
        print("F")
        print("G")
        okC.release()
    }
}
R_5 = Thread.start {
    N_5.times {
        okH.acquire()
        print("H")
        print("I")
    }
}

P_5.join()
Q_5.join()
R_5.join()
println()