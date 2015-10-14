/****************************************************************************
 *                     Copyright (C) 2015, AdaCore                          *
 *                                                                          *
 * This library is free software;  you can redistribute it and/or modify it *
 * under terms of the  GNU General Public License  as published by the Free *
 * Software  Foundation;  either version 3,  or (at your  option) any later *
 * version. This library is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 ****************************************************************************/

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
   extern void ada_print_time(void* output, double elapsed);
   extern void ada_start_line(void* output, const char* title);
}

void test_cpp(void * output) {
   ada_start_line(output, "C++");

   std::list<int>  v;

   std::clock_t begin = clock();
   for (int c = 1; c <= items_count - 2; c++) {
      v.push_back(2);
   }
   v.push_back(5);
   v.push_back(6);
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   begin = clock();
   std::list<int> v_copy (v);
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   begin = clock();
   int count = 0;
   std::list<int>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (*it > 3) {
         count ++;
      }
      it ++;
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }

   count = 0;
   for (auto e : v) {
      if (e > 3) {
         count ++;
      }
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }


   begin = clock();
   count = std::count_if (v.begin(), v.end(), IsGreaterThan3);
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != 2) {
      std::cout << "C++ error while counting" << std::endl;
   }

}

void test_cpp_string(void * output) {
   std::list<std::string>  v;

   ada_start_line(output, "C++");

   std::clock_t begin = clock();
   for (int c = 1; c <= items_count; c++) {
      v.push_back("str1");
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   begin = clock();
   std::list<std::string> v_copy (v);
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   begin = clock();
   int count = 0;
   std::list<std::string>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (startsWithStr(*it)) {
         count ++;
      }
      it ++;
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }


   count = 0;
   for (std::string& e : v) {
      if (startsWithStr(e)) {
         count ++;
      }
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }


   begin = clock();
   count = std::count_if (v.begin(), v.end(), startsWithStr);
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

}

extern "C" {
   void test_c_int(void* output) {
      test_cpp(output);
   }
   void test_c_str(void* output) {
      test_cpp_string(output);
   }
}
