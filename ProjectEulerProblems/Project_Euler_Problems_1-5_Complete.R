### Project Euler Problems 1-5
### Eva Biedron
### 7/27/2022 through 8/17/2022

# I originally solved these problems in python (see my Github) using only my own
# knowledge. After entering a correct answer on the Project Euler website, I
# gained access to overview documentation regarding the solution and avenues for
# optimization. When coding this iteration of solutions, I implemented a 
# combination of my own solutions and those outlined in general in the 
# documentation for each problem. I followed the tidyverse style guide, availble
# at https://style.tidyverse.org/.

# Problem 1 -------------------------------------------------------------------- 
# Find the sum of all the multiples of 3 or 5 below 1000.

# Helper function
sum_all_numbers_divisible_by <- function(n) {
  p <- maximum %/% n # Dividing by n.
  sum_to_p <- floor(p * (p + 1) / 2) # This is the sum from one to p.
  sum_of_numbers <- floor(n * sum_to_p)   # Multiplying by n.
  # return(sum_of_numbers) # Unnecessary, see https://style.tidyverse.org/
  # functions.html#return.
}

# Parameters
maximum <- 999 # Problem parameters say below, but not including, 1000.

# Calculations
p1_answer <- (sum_all_numbers_divisible_by(3) 
            + sum_all_numbers_divisible_by(5) 
            - sum_all_numbers_divisible_by(15)) # Removing multiples of both numbers 
            # added a second time.
print(p1_answer)

# Problem 2 --------------------------------------------------------------------
# Find the sum of all the even terms in the Fibonacci sequence < 4 X 10^6.

# Parameters
maximum <- 4000000
sum_of_cs <- 0 
a <- 1 
b <- 1 
c <- strtoi(a + b) 

# Calculations
# This loop calculates the Fibonacci sequence, but only adds every third value.
# Every third number in a Fibonacci sequence is even.
while (c < maximum) {
  sum_of_cs <- sum_of_cs + c # Add previous loops' c value to sum.
  a <- b + c 
  b <- c + a 
  c <- a + b 
}
p2_answer <- sum_of_cs
print(p2_answer)

# Problem 3 -------------------------------------------------------------------- 
# Find the largest prime factor of a composite number.

# Helper function
factor_out_two <- function(n) {
  if (n %% 2 == 0) {
    n <- n %/% 2
    last_fact <- 2
    while (n %% 2 == 0) {
      n <- n %/% 2
    } 
  } else {
    last_fact <<- 1 # Global assignment needs to be here when final calcs done.
  }
  n <<- n # Global assignment, returning does not update value outside f(x).
}

# Parameters
n <- 600851475143

# Calculations
factor_out_two(n) # Use previously written helper function.
fact <- 3 # Start at first odd number.

# The second largest prime factor of a number will be less than it's square root 
# because there will only be one prime factor greater than the square root.
max_fact <- sqrt(n)

# Only check up to that penultimate prime factor.
while ((n > 1) && (fact <= max_fact)) {
  if (n %% fact == 0) {
    n <- n %/% fact
    last_fact <- fact
    while (n %% fact == 0) {
      n <- n %/% fact
    }
    max_fact <- sqrt(n)
  }
  fact <- fact + 2 # Only check odd numbers i.e. 3+2x.
}

# If the last prime is discovered and completely divided out multiple times 
# (meaning the last and second to last prime factors are the same), that 
# is the largest prime factor.
if (n == 1) {
  p3_answer <- last_fact
} else {
  # Otherwise, the final prime will be the only thing not divided out of n.
  p3_answer <- n
}

print(p3_answer)

# Problem 4 --------------------------------------------------------------------
# Find the largest palindrome made from the product of two 3-digit numbers.

# Helper functions
reverse_vector <- function(n) {
  n_chars <- toString(n)
  n_split <- strsplit(n_chars, "")[[1]] # Split into a vector rev() can accept.
  n_reversed <- rev(n_split)
  n_final <- paste(n_reversed, collapse = "")
}  

is_palindrome <- function(m) {
  return (m == reverse_vector(m)) 
}

# Parameters
a <- 999
largest_palindrome <- 0

# Calculations
# Counting down, find the largest palindromic product and then stop.
while (a >= 100) {
  b <- 999
  while(b >= a) {
    if ((a*b) <= largest_palindrome) { # Check if largestPal was updated once already.
      break
    }
    if (is_palindrome(a * b)) { # Otherwise, update if product is palindromic.
      largest_palindrome <- a * b
    }
    b <- b - 1  
  }
  a <- a - 1
}

p4_answer <- largest_palindrome
print(p4_answer)

# Problem 5 --------------------------------------------------------------------
# What is the smallest positive number that is evenly divisible by all of the 
# numbers from 1 to 20?

# Helper function(s)

find_primes_under <- function(n) {
  # Uses a sieve of Eratosthenes
  
  candidates <- rep(TRUE, times = n)
  
  for (i in 2:n) { # 2:20
    if (candidates[i] == TRUE) { # Check if next prime
      for (j in ((i + 1):n)) { # Don't check the current prime found
        if (j %% i == 0) { # Check for multiples
          if (candidates[j] == TRUE) { # Check if previously checked
            candidates[j] <- FALSE # New non-prime
          }
        }
      }
    }
  }
  
  primes <- c() # Hold the found prime numbers.
  
  for (i in 2:n) { # Find indices indicated as primes
    if (candidates[i] == TRUE) {
      primes <- append(primes, i) # Add to output list.
    }
  }
  primes <<- primes # Make available globally
}

find_primes_under(24)
p <- primes

k <- 20
N <- 1
i <- 1
check <- TRUE
limit <- sqrt(k)

while (p[i] <= k) {
  a_i <- 1
  if (check == TRUE) {
    if (p[i] <= limit) {
      a_i <- floor(log(k) / log(p[i]))
    } else {
      check <- FALSE
    }
  }
  N <- (N * (p[i])^a_i) 
  i <- i + 1
}

p5_answer <- N
print(p5_answer)