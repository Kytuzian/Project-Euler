#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

bool is_prime(int n)
{
    if (n == 1) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;

    for (int i = 3; i*i <= n; i += 2)
    {
        if (n % i == 0)
            return false;
    }

    return true;
}

// Constructs a list of digits from a number
// digits(8234) = {8,2,3,4}
vector<int> digits(int n)
{
    vector<int> result;

    while (n > 0)
    {
        result.insert(result.begin(), n % 10);
        n /= 10;
    }

    return result;
}

// Constructs a number from a list of digits
// from_digits({8,2,3,4}) = 8234
int from_digits(vector<int> n_digits)
{
    int result = 0;
    for (auto const &i : n_digits)
        result = result * 10 + i;

    return result;
}

bool is_circular_prime(int n)
{
    vector<int> n_digits = digits(n);

    // We don't need to check the last rotation, because we already verified n was prime earlier
    for (int i = 0; i < n_digits.size() - 1; i++)
    {
        rotate(n_digits.begin(), n_digits.begin() + 1, n_digits.end());

        // If the current rotation of digits isn't prime, this can't possible be a circular prime
        if (!is_prime(from_digits(n_digits)))
            return false;
    }

    return true;
}

int main(int argc, char **argv)
{
    int count = 0;

    for (int i = 0; i < 1000000; i++)
    {
        // Can't be a circular prime if it isn't prime in the first place.
        if (is_prime(i))
        {
            bool is_circular = is_circular_prime(i);

            if (is_circular)
            {
                cout << i << " is circular." << endl;
                count++;
            }
        }
    }

    cout << endl;
    cout << "Found " << count << " circular primes." << endl;

    return 0;
}

