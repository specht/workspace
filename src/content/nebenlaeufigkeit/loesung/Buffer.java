public class Buffer {
    private final int[] items;
    private int count = 0;
    private int first = 0;

    public Buffer(int capacity) {
        items = new int[capacity];
    }

    public synchronized void put(int x) throws InterruptedException {
        while (count == items.length) {
            wait();
        }
        items[(first + count) % items.length] = x;
        count++;
        notifyAll();
    }

    public synchronized int take() throws InterruptedException {
        while (count == 0) {
            wait();
        }
        int x = items[first];
        first = (first + 1) % items.length;
        count--;
        notifyAll();
        return x;
    }

    public static int transfer(int n, int capacity) throws InterruptedException {
        Buffer buffer = new Buffer(capacity);
        int[] sum = { 0 };
        Thread producer = new Thread(() -> {
            try {
                for (int i = 1; i <= n; i++) buffer.put(i);
            } catch (InterruptedException e) {
                return;
            }
        });
        Thread consumer = new Thread(() -> {
            try {
                for (int i = 1; i <= n; i++) sum[0] += buffer.take();
            } catch (InterruptedException e) {
                return;
            }
        });
        producer.start();
        consumer.start();
        producer.join();
        consumer.join();
        return sum[0];
    }
}
