import java.util.concurrent.Semaphore

Semaphore aDone = new Semaphore(0)
Semaphore bDone = new Semaphore(0)

def opA() {
    println "Operation A completed"
}

def opB() {
    println "Operation B completed"
}

def opC() {
    println "Operation C completed"
}

A = Thread.start {
    opA()
    aDone.release()
}

B = Thread.start {
    opB()
    bDone.release()
}

C = Thread.start {
    aDone.acquire()
    bDone.acquire()
    opC()
}

A.join()
B.join()
C.join()
println()