// Peter Rauscher
// I pledge my honor that I have abided by the Stevens Honor System.
import java.util.concurrent.Semaphore
// One-time use barrier
// Barrier size = N
// Total number of threads in the system = N

final int N=3
int t=0
Semaphore barrier = new Semaphore(0)
Semaphore mutex = new Semaphore(1)
Semaphore restart = new Semaphore(0)
c = new int[N]
N.times {
    int id = it
    Thread.start {
	while (true) {
	    // barrier arrival protocol
	    mutex.acquire()
	    c[id]++
	    if (t<N) {
		t++
		if (t==N) {
		    N.times { barrier.release() }
		}
	    }
	    mutex.release()
	    // barrier
	    println id+" got to barrier. c="+c[id]
	    barrier.acquire()
		println id+" went through. c="+c[id]
		mutex.acquire()
		t--
		if (t == 0) {
			N.times { restart.release() }
		}
		mutex.release()
		restart.acquire()
		}
    }
}
