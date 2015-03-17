#include <algorithm>
#include <iostream>
#include <ctime>
#include <list>

bool IsGreaterThan3 (int i) { return i > 3; }

void test_cpp() {
   std::list<int>  v;

   std::clock_t begin = clock();
   for (int c = 1; c <= 10000000; c++) {
      v.push_back(2);
   }
   v.push_back(5);
   v.push_back(6);
   double elapsed_secs = double(clock() - begin) / CLOCKS_PER_SEC;
   std::cout << "C++ Fill V  => " << elapsed_secs << "\n";


   begin = clock();
   int count = 0;
   for (int repeat = 0; repeat < 10; repeat ++) {
      std::list<int>::const_iterator it (v.begin());
      while (it != v.end()) {
         if (*it > 3) {
            count ++;
         }
         it ++;
      }
   }
   elapsed_secs = double(clock() - begin) / CLOCKS_PER_SEC;
   std::cout << "C++ Count " << count << " in " << elapsed_secs << std::endl;


   begin = clock();
   count = 0;
   for (int repeat = 0; repeat < 10; repeat ++) {
      count += std::count_if (v.begin(), v.end(), IsGreaterThan3);
   }
   elapsed_secs = double(clock() - begin) / CLOCKS_PER_SEC;
   std::cout << "C++ Count with count_if " << count << " in " << elapsed_secs << std::endl;
}

extern "C" {
   void test_c() {
      test_cpp();
   }
}
