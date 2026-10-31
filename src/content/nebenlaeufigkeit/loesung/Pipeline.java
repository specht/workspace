import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

public class Pipeline {
    public static int transfer(int n, int capacity) throws InterruptedException {
        BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(capacity);
        int[] sum = { 0 };
        Thread producer = new Thread(() -> {
            try {
                for (int i = 1; i <= n; i++) queue.put(i);
            } catch (InterruptedException e) {
                return;
            }
        });
        Thread consumer = new Thread(() -> {
            try {
                for (int i = 1; i <= n; i++) sum[0] += queue.take();
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
