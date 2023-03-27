import java.util.concurrent.Semaphore

Seamphore tickets = new Semaphore(0)
Semaphore mutex = new Semaphore(1)

20.times { // Patriots
    tickets.release()
    // fan goes into the bar
}

20.times { // Jets
    mutex.acquire()
    tickets.acquire()
    tickets.acquire()
    mutex.release()
    // fan goes into the bar
}