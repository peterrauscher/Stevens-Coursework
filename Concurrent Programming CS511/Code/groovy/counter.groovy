class Counter {
    private int c
    Counter (int n) { c = n }
    public void inc() { synchronized (this) { c++ } }
    public synchronized void dec() { c-- }
}

Counter c = new Counter(0)

P = Thread.start {
    10.times {
        c.inc()
    }
}

Q = Thread.start {
    10.times {
        c.inc()
    }
}

P.join()
Q.join()
println c.c