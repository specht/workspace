use std::fs::File;
use std::io::Read;

// To use real random numbers, we would have to create a Rust
// project with cargo that uses the `rand` crate. For simplicity,
// we will just use `/dev/urandom` as a source of random numbers.

fn rand(min: u32, max: u32) -> u32 {
    let mut file = File::open("/dev/urandom").expect("Failed to open /dev/urandom");
    let mut buffer = [0u8; 4];
    file.read_exact(&mut buffer).expect("Failed to read from /dev/urandom");
    let random_number = u32::from_le_bytes(buffer);
    min + (random_number % (max - min + 1))
}

fn bubble_sort(arr: &mut [i32]) {
    let n = arr.len();
    for i in 0..n {
        for j in 0..n - i - 1 {
            if arr[j] > arr[j + 1] {
                arr.swap(j, j + 1);
            }
        }
    }
}

fn main() {
    let mut arr = [0; 10];
    for i in 0..10 {
        arr[i] = rand(1, 100) as i32;
    }

    println!("Original array: {:?}", arr);
    bubble_sort(&mut arr);
    println!("Sorted array: {:?}", arr);
}