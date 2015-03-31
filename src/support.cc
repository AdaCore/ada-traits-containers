#include <algorithm>
#include <iostream>
#include <iomanip>
#include <ctime>
#include <list>
#include <string>

bool IsGreaterThan3 (int i) { return i > 3; }
bool startsWithStr (const std::string& s) { return s[0] == 's'; }

extern "C" {
   extern const int items_count;
   extern void _ada_print_time(double elapsed);
   extern void _ada_start_line(const char* title);
}

void test_cpp() {
   _ada_start_line("C++");

   std::list<int>  v;

   std::clock_t begin = clock();
   for (int c = 1; c <= items_count - 2; c++) {
      v.push_back(2);
   }
   v.push_back(5);
   v.push_back(6);
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);


   begin = clock();
   int count = 0;
   std::list<int>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (*it > 3) {
         count ++;
      }
      it ++;
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }

   count = 0;
   for (auto e : v) {
      if (e > 3) {
         count ++;
      }
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }


   begin = clock();
   count = std::count_if (v.begin(), v.end(), IsGreaterThan3);
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }

   begin = clock();
   std::list<int> v_copy (v);
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
}

void test_cpp_string() {
   std::list<std::string>  v;

   _ada_start_line("C++");

   std::clock_t begin = clock();
   for (int c = 1; c <= items_count; c++) {
      v.push_back("str1");
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);


   begin = clock();
   int count = 0;
   std::list<std::string>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (startsWithStr(*it)) {
         count ++;
      }
      it ++;
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }


   count = 0;
   for (auto e : v) {
      if (startsWithStr(e)) {
         count ++;
      }
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }


   begin = clock();
   count = std::count_if (v.begin(), v.end(), startsWithStr);
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

   begin = clock();
   std::list<std::string> v_copy (v);
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
}

extern "C" {
   void test_c_int() {
      test_cpp();
   }
   void test_c_str() {
      test_cpp_string();
   }
}
