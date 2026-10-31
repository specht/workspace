import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class Primes {
    public static int countPrimes(int from, int to) {
        int count = 0;
        for (int n = Math.max(from, 2); n < to; n++) {
            boolean prime = true;
            for (int d = 2; d * d <= n; d++) {
                if (n % d == 0) {
                    prime = false;
                    break;
                }
            }
            if (prime) count++;
        }
        return count;
    }

    public static int countPrimesParallel(int limit, int parts) throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(parts);
        int size = (limit + parts - 1) / parts;
        List<Future<Integer>> results = new ArrayList<>();
        for (int p = 0; p < parts; p++) {
            int from = p * size;
            int to = Math.min(limit, (p + 1) * size);
            results.add(pool.submit(() -> countPrimes(from, to)));
        }
        int sum = 0;
        for (Future<Integer> result : results) {
            sum += result.get();
        }
        pool.shutdown();
        return sum;
    }
}
