use std::io;
use std::io::Write;

fn main() {
    print!("Enter a number: ");
    io::stdout().flush().unwrap();
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Failed to read input");
    let number: u64 = input.trim().parse().expect("Invalid input");

    print!("Prime factors of {}: ", number);
    let mut n = number;
    let mut factor = 2;

    while n > 1 {
        if n % factor == 0 {
            print!("{} ", factor);
            n /= factor;
        } else {
            factor += 1;
        }
    }
    println!();
}