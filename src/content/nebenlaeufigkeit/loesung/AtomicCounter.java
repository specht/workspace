import java.util.concurrent.atomic.AtomicInteger;

public class AtomicCounter {
    public static int countWithThreads(int threads, int times) throws InterruptedException {
        AtomicInteger count = new AtomicInteger();
        Thread[] all = new Thread[threads];
        for (int t = 0; t < threads; t++) {
            all[t] = new Thread(() -> {
                for (int i = 0; i < times; i++) count.incrementAndGet();
            });
            all[t].start();
        }
        for (Thread thread : all) thread.join();
        return count.get();
    }
}
