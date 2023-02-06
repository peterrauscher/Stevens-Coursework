import java.util.concurrent.Semaphore

Semaphore blockZ = new Semaphore(0)

int y = 0, z = 0
P_4 = Thread.start {
    int x
    x = y + z
    print("x=" + x)
}

Q_4 = Thread.start {
    y = 1
    z = 2
}

// 1. What are the possible final values for x?
// A: The possible final values for x are 0, 1, and 3

// 2. Is it possible to use semaphores to restrict the set
// of possible values of x to be just two possible values?
// A: No, it would not be possible, assuming that both threads
// must run to completion. Otherwise, you would have to indefinitely
// block the assignment of 2 to z to restrict the set of possible
// values of x to 0 or 1

println()
