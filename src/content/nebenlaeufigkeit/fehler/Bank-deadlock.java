class Account {
    final int id;
    int balance;

    Account(int id, int balance) {
        this.id = id;
        this.balance = balance;
    }
}

public class Bank {
    static void transfer(Account from, Account to, int amount) {
        // each thread takes its own account's lock first, and waits a
        // moment: then both hold one and wait for the other, every time
        Account first = from;
        Account second = to;
        synchronized (first) {
            try { Thread.sleep(1); } catch (InterruptedException e) { return; }
            synchronized (second) {
                from.balance -= amount;
                to.balance += amount;
            }
        }
    }

    static int run(int times) throws InterruptedException {
        Account x = new Account(1, 100);
        Account y = new Account(2, 100);
        Thread a = new Thread(() -> { for (int i = 0; i < times; i++) transfer(x, y, 1); });
        Thread b = new Thread(() -> { for (int i = 0; i < times; i++) transfer(y, x, 1); });
        a.start();
        b.start();
        a.join();
        b.join();
        return x.balance + y.balance;
    }

    static void main() throws InterruptedException {
        IO.println(run(100000));
    }
}
