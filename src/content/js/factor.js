const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

function getPrimeFactors(num) {
    let factors = [];
    let divisor = 2;

    while (num > 2) {
        if (num % divisor === 0) {
            factors.push(divisor);
            num = num / divisor;
        } else {
            divisor++;
        }
    }

    return factors;
}

rl.question('Enter a number: ', (num) => {
    const primeFactors = getPrimeFactors(parseInt(num));
    console.log('Prime factors:', primeFactors);
    rl.close();
});