import java.util.concurrent.Semaphore

MAX_WEIGHTS = 10
GYM_CAP = 50

Semaphore ticketToGym = new Semaphore(GYM_CAP)
Semaphore weightDiscs = new Semaphore(MAX_WEIGHTS)
Semaphore weightsMutex = new Semaphore(1)
okToUseApparatus = [
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1),
    new Semaphore(1)
]

def make_routine(int no_exercises) { // returns a random routine
    Random rand = new Random()
    int size = rand.nextInt(no_exercises)
    def routine = []

    size.times {
        routine.add(new Tuple(rand.nextInt(4), rand.nextInt(MAX_WEIGHTS)))
    }
    return routine
}

1000.times {
    int id = it
    Thread.start { // client
        def routine = make_routine(20)
        ticketToGym.acquire() // enter gym
        println "$id is in the gym:"
        routine.size().times {
            weightsMutex.acquire()
            routine[it][1].times { weightDiscs.acquire() }
            okToUseApparatus[routine[it][0]].acquire()
            weightsMutex.release()
            // complete exercise on machine
            println "$id is performing:"+routine[it][0]+"--"+routine[it][1]
            okToUseApparatus[routine[it][0]].release()
            routine[it][1].times { weightDiscs.release() }
        }
        ticketToGym.release() // leave gym
        println "$id left the gym:"
    }
}

return

return