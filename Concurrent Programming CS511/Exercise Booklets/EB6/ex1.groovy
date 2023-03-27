import java.util.concurrent.locks.*

class Bar {
    int jetsFans = 0
    int patsFans = 0
    boolean isLate = false
    Lock lock = new ReentrantLock()
    Condition jetCanGo = lock.newCondition()

    void jets() {
        lock.lock()
        try {
            while (jetsFans < (patsFans / 2) || !isLate) {
                jetCanGo.await()
            }
            jetsFans++
            println("Jets: " + jetsFans + " | Pats: " + patsFans)
        } finally {
            lock.unlock()
        }
    }

    void patriots() {
        lock.lock()
        try {
            patsFans++
            println("Jets: " + jetsFans + " | Pats: " + patsFans)
            jetCanGo.signal()
        } finally {
            lock.unlock()
        }
    }

    void itGotLate() {
        lock.lock()
        try {
            isLate = true
            println("It got late!")
            jetCanGo.signalAll()
        } finally {
            lock.unlock()
        }
    }
}

Bar b = new Bar () ;
100.times {
    Thread.start { // jets
        b.jets() ;
    }
}
100.times {
Thread.start { // patriots
        b.patriots () ;
    }
}
    Thread.start {
        while(true) {
            if ((Math.random() * 100) < 10 && !b.isLate) {
                b.itGotLate()
            }
        }
    }