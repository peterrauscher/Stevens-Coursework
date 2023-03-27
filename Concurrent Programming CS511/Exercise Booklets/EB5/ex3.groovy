import java.util.concurrent.Semaphore

N = 20 // Ferry capacity
int numberAboard = 0 // Number of passengers on board
ticketToRide = [new Semaphore(0), new Semaphore(0)]
ticketToLeave = [new Semaphore(0), new Semaphore(0)]
setSail = new Semaphore(0)

Thread.start { // Ferry
    int coast = 0
    while (true) {
        // allow passengers on
        N.times { ticketToRide[coast].release() }
        setSail.acquire()
        // move to opposite coast
        coast = 1 - coast
        // wait for all passengers to get off
        N.times { ticketToLeave[coast].release() }
    }
}

1000.times {
    Thread.start { // Passenger on East coast
        // get on at East Coast
        if (coast == 0) {
            ticket.acquire()
        }
        // get off at opposite coast
    }
}

1000.times {
    Thread.start { // Passenger on West coast
        // get on
        // get off at opposite coast
    }
}