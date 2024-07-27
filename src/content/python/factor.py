num = int(input("Enter a number: "))
print(f"Prime factors of {num} are: ", end="")
i = 2
while i <= num:
    if num % i == 0:
        print(i, end=' ')
        num = num / i
    else:
        i += 1
print()