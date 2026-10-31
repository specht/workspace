public class Counter {
    private int count = 0;

    public synchronized void increment() {
        count++;
    }

    public synchronized int get() {
        return count;
    }

    public static int countWithThreads(int threads, int times) throws InterruptedException {
        Counter counter = new Counter();
        Thread[] all = new Thread[threads];
        for (int t = 0; t < threads; t++) {
            all[t] = new Thread(() -> {
                for (int i = 0; i < times; i++) {
                    counter.increment();
                }
            });
            all[t].start();
        }
        for (Thread thread : all) {
            thread.join();
        }
        return counter.get();
    }
}
