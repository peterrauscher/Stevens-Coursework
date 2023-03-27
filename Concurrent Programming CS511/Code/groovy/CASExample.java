import java.util.concurrent.atomic.AtomicInteger;

public class CASExample {
    public static void main(String args[]) {
        for (int i = 0; i < 100000; i++) {
            test();
        }
    }

    private static void test() {
        System.out.println("Initial value is: 0");
        AtomicInteger val = new AtomicInteger(0);
        val.compareAndSet(0, 6);
        System.out.println("Atomically compared and swapped! New value is: 6");
    }
}