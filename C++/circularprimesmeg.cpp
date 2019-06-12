#include <iostream>
using namespace std; 
#include <math.h>
#include <vector>

bool is_prime(int num);
void rotations(vector<int> &rotNums, int num);
bool isCircPrime(int num);
bool isGood(int num);
void initialize(int nums[], int power);
void leftshift(int a[], int sz);

// this is the number of digits under 10^6 without a 0,2,4,6,8, or 5 in them
// size = pow(4,6) + pow(4,5) + ... + pow(4,1)
const int size = 5460; 

int main() {
	int highestPower = 6; // because one million is 10^6
	int numCircPrimes = 0;
	int nums[size] = {0};

	initialize(nums, highestPower);

	cout << "The circular primes are: " << endl;

	// list the circular primes
	for (int i = 0; i < size; i++) {
		if (isCircPrime(nums[i])) {
		 	cout << nums[i] << endl;
			numCircPrimes++;
		}
	}

	// display total number of circular primes
	cout << "Num of circular primes under " << pow(10, highestPower) << " is: " << numCircPrimes << endl;

	return 0;
}


// checks if a number if prime using trial division up to the square root of the number
bool is_prime(int num) {
	if (num == 1) return false;
	if (num == 2) return true;

	for (int i = 2; i <= sqrt(num); i++) {
		if (num % i == 0) 
			return false;
	}
	return true;
}

// checks if a number is a circular prime by calling the function to get all 
// rotations of the number and checking if each of these numbers is prime
bool isCircPrime(int num) {
	bool result = true;
	vector<int> rotNums;
	rotations(rotNums, num);

	for (auto it = rotNums.begin(); it < rotNums.end(); it++) {
		result = result && is_prime(*it);
	}		
	
	return result;
}

// gets all the rotations of each number by making a list of the digits
// and then adding the digits multiplied by a certain power of 10 after
// performing a circular shift on the list for each loop 
void rotations(vector<int> &rotNums, int num) {
	int numDig = log10(num) + 1;
	int digList[numDig];
	int newNum = 0;
	int temp = 0;
	rotNums.clear();

	for (int i = (numDig - 1); i >= 0; i--) {
		temp = num / pow(10,i);
		digList[i] = temp;
		num = num - temp * pow(10,i);
	}

	for (int i = 0; i < numDig; i++) {
		newNum = 0;
		for (int j = 0; j < numDig; j++) { 
			newNum += digList[j] * pow(10, j);
		}
		rotNums.push_back(newNum);
		leftshift(digList, numDig);		
	}

}

// shifts the list circularly in the left direction
// Ex: 
// 		leftshift([1,2,3,4,5], 5) 
// 		returns [2,3,4,5,1]
void leftshift(int a[], int sz)
{
        int firstIndex = a[0];

        for (int i = 0; i < sz - 1; i++) {
                a[i] = a[i+1];
        }

        a[sz-1] = firstIndex;
}


// checks if a number has a 0,2,4,6,8, or 5 in it because 
// if it does, the number can't possibly be a circular prime
// and is then dubbed "bad"; otherwise, number is "good" 
bool isGood(int num) {
	int numDig = log10(num) + 1;
   	int digList[numDig];
	int temp; 

	if (num == 2) return true;
	if (num == 5) return true;

	for (int i = (numDig - 1); i >= 0; i--) {
		temp = num / pow(10,i);
		digList[i] = temp;
		num = num - temp * pow(10,i);
		
	} 	

	for (int i = 0; i < numDig; i++) {
		if (digList[i] == 2 || digList[i] == 4 || digList[i] == 6 || digList[i] == 8 || digList[i] == 0 || digList[i] == 5) {
			return false;
		}
	} 

	return true;
}

void initialize(int nums[], int power) {
	int index = 0;

	for (int i = 1; i <= power; i++) {
		for (int j = pow(10, i - 1); j < pow(10, i); j++) {
			if (isGood(j)) {
				nums[index] = j;
				index++;
			}
		}
	}
}


