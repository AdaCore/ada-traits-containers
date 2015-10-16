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

bool IsLessEqual2 (int i) { return i <= 2; }
bool startsWithStr (const std::string& s) { return s[0] == 'f'; }

extern "C" {
   extern const int items_count;
   extern void start_container_test
      (void* output, const char *base, const char* elements,
       const char *nodes, const char* container, const char *e_type);
   extern void end_container_test (void* output);
   extern void start_test (void* output, const char* name);
   extern void end_test (void* output);
}

extern "C"
void test_cpp_int(void * output) {
   start_container_test (output, "C++", "", "", "std::list", "Integer");

   std::list<int>  v;

   start_test (output, "fill");
   for (int c = 1; c <= items_count; c++) {
      v.push_back(2);
   }
   end_test (output);

   start_test (output, "copy");
   std::list<int> v_copy (v);
   end_test (output);

   int count = 0;
   start_test (output, "cursor loop");
   std::list<int>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (*it <= 2) {
         count ++;
      }
      it ++;
   }
   end_test (output);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

   count = 0;
   start_test (output, "for-of loop");
   for (auto e : v) {
      if (e <= 2) {
         count ++;
      }
   }
   end_test (output);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

   start_test (output, "count_if");
   count = std::count_if (v.begin(), v.end(), IsLessEqual2);
   end_test (output);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

   end_container_test (output);
}

extern "C"
void test_cpp_string(void * output) {
   start_container_test (output, "C++", "", "", "std::list", "String");

   std::list<std::string>  v;

   start_test (output, "fill");
   for (int c = 1; c <= items_count; c++) {
      v.push_back("foo");
   }
   end_test (output);

   start_test (output, "copy");
   std::list<std::string> v_copy (v);
   end_test (output);

   int count = 0;
   start_test (output, "cursor loop");
   std::list<std::string>::const_iterator it (v.begin());
   while (it != v.end()) {
      if (startsWithStr(*it)) {
         count ++;
      }
      it ++;
   }
   end_test (output);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

   count = 0;
   start_test (output, "for-of loop");
   for (std::string& e : v) {
      if (startsWithStr(e)) {
         count ++;
      }
   }
   end_test (output);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

   start_test (output, "count_if");
   count = std::count_if (v.begin(), v.end(), startsWithStr);
   end_test (output);
   if (count != items_count) {
      std::cout << "C++ error while counting" << std::endl;
   }

   end_container_test (output);
}
