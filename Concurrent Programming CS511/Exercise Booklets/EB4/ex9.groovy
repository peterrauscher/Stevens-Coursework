import java.util.concurrent.Semaphore

int counter = 0
Semaphore mutex = new Semaphore(1)
Semaphore s = new Semaphore(-1)

Thread.start {
    10.times {
        mutex.acquire()
        counter++
        mutex.release()
    }
    s.release()
}

Thread.start {
    10.times {
        mutex.acquire()
        counter++
        mutex.release()
    }
    s.release()
}

s.acquire()
print counter

/**
Q: Show that if we remove the condition permissions<0, then we cannot allow negative permits. An
example of what can go wrong follows. This code attempts to ensure that the value of counter is
only printed after both turnstile threads finish. It relies on negative permissions and the modified
pseudocode for release. It is possible for the code to print the value of counter before both
turnstile have finished. Why?

A: If we remove the check for permissions < 0, then if s.acquire() is run before either thread
releases, the acquire() will add the process in a blocked state, and the first
release which is run will determine that processes.empty() is not true,
and thus it will wake the process from being blocked and place it in ready state,
allowing the process to continue. Thus, the print(counter) could be run 
without waiting for both (or either) thread(s) to finish.
*/