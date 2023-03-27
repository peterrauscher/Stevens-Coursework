class PC {
    Integer buffer

    PC() {
        buffer = null
    }

    public synchronized void produce(Integer o) {
        while (buffer!=null) {
            wait()
        }
        buffer = o
        notifyAll()
    }

    public synchronized Integer consume() {
        Integer temp
        while (buffer==null) {
            wait()
        }
        temp = buffer
        notifyAll()
        return temp
    }
}

PC b = new PC()

10.times {
    int id = it
    Thread.start { // Producer
        Integer i = (new Random()).nextInt(1000)
        b.produce(i)
        println "$id produced $i."
    }
}

10.times {
    int id = it
    Thread.start { // Consumer
        Integer i = b.consume()
        println "$id consumed $i."
    }
}