import java.util.concurrent.locks.*
// Peter Rauscher
// I pledge my honor that I have abided by the Stevens Honor System

class Pizza {
    int smallPizzaCount = 0
    int largePizzaCount = 0
    Lock lock = new ReentrantLock()
    Condition largeIsAvailable = lock.newCondition()
    Condition smallIsAvailable = lock.newCondition()

    private printState(String prefix) {
        println(prefix + " -> " + smallPizzaCount + " small | " + largePizzaCount + " large")
    }

    void purchaseSmallPizza() {
        lock.lock()
        try {
            while (smallPizzaCount < 1) {
                smallIsAvailable.await()
            }
            smallPizzaCount--
            printState("Bought a small")
        } finally {
            lock.unlock()
        }
    }

    void purchaseLargePizza() {
        lock.lock()
        try {
            while (largePizzaCount < 1 && smallPizzaCount < 2) {
                largeIsAvailable.await()
            }
            if (largePizzaCount > 0) {
                largePizzaCount--
                printState("Bought a large")
            } else if (smallPizzaCount >= 2) {
                smallPizzaCount -= 2
                printState("Bought 2 smalls")
            }
        } finally {
            lock.unlock()
        }
    }

    void bakeSmallPizza() {
        lock.lock()
        try {
            smallPizzaCount++
            smallIsAvailable.signal()
            largeIsAvailable.signal()
            printState("Baked a small")    
        } finally {
            lock.unlock()
        }
    }

    void bakeLargePizza() {
        lock.lock()
        try {
            largePizzaCount++
            largeIsAvailable.signal()
            printState("Baked a large")
        } finally {
            lock.unlock()
        }
    }
}

Pizza p = new Pizza()

100.times { // 100 clients
    Thread.start {
        if ((new Random()).nextInt(2)== 0)
            { p.purchaseSmallPizza() }
        else 
            { p.purchaseLargePizza() }
    }
}

10.times { // 10 bakers
    Thread.start {
        10.times { // 10 pizzas each baker
            if ((new Random()).nextInt(2) == 0)
            { p.bakeSmallPizza() }
            else 
            { p.bakeLargePizza() }
        }
    }
}