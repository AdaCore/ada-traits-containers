#include <algorithm>
#include <iostream>
#include <iomanip>
#include <ctime>
#include <list>
#include <string>

bool IsGreaterThan3 (int i) { return i > 3; }
bool startsWithStr (const std::string& s) { return s[0] == 's'; }

extern const int items_count;

void print_time(double elapsed) {
   if (elapsed == 0.0) {
      std::cout << "             |";
   } else {
      std::cout << " " << std::fixed << std::setprecision(5) << elapsed << "     |";
   }
}

void test_cpp() {
   std::list<int>  v;

   std::clock_t begin = clock();
   for (int c = 1; c <= items_count; c++) {
      v.push_back(2);
   }
   v.push_back(5);
   v.push_back(6);
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);


   begin = clock();
   int count = 0;
   std::list<int>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (*it > 3) {
         count ++;
      }
      it ++;
   }
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }

   count = 0;
   for (auto e : v) {
      if (e > 3) {
         count ++;
      }
   }
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }


   begin = clock();
   count = std::count_if (v.begin(), v.end(), IsGreaterThan3);
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }
}

void test_cpp_string() {
   std::list<std::string>  v;

   std::clock_t begin = clock();
   for (int c = 1; c <= items_count; c++) {
      v.push_back("str1");
   }
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);


   begin = clock();
   int count = 0;
   std::list<std::string>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (startsWithStr(*it)) {
         count ++;
      }
      it ++;
   }
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }


   count = 0;
   for (auto e : v) {
      if (startsWithStr(e)) {
         count ++;
      }
   }
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }


   begin = clock();
   count = std::count_if (v.begin(), v.end(), startsWithStr);
   print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }
}

extern "C" {
   void test_c_int() {
      test_cpp();
   }
   void test_c_str() {
      test_cpp_string();
   }
}
