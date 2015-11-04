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
#include <map>
#include <unordered_map>

extern "C" {
   extern const int items_count;
   extern const int repeat_count;
   extern void start_container_test
      (void* output, const char *base, const char* elements,
      const char *nodes, const char* category, int favorite);
   extern void save_container_size (void* output, long int size);
   extern void end_container_test
      (void* output, int allocated, int allocs_count, int frees_count);
   extern void start_test (void* output, const char* name);
   extern void end_test
      (void* output, int allocated, int allocs_count, int frees_count);
}

bool IsLessEqual2 (int i) { return i <= 2; }
bool IsEqualItemsCount (int i) { return i == items_count; }
bool startsWithB (const std::string& s) { return s[0] == 'b'; };
bool startsWithStr (const std::string& s) { return s[0] == 'f'; }
bool valueStartsWithStr (const std::pair<std::string, std::string> s) {
   return s.second[0] == 'f';
}
bool valueStartsWithF (const std::pair<std::string, std::string> s) {
   return s.second[0] == 'f';
}

/**
 * Counting the number of allocations and frees.
 * From
 */
int number_of_allocs = 0;
int number_of_frees = 0;
std::size_t total_allocated = 0;

void reset_mem() {
   number_of_allocs = 0;
   number_of_frees = 0;
   total_allocated = 0;
}

void* operator new(std::size_t size) throw(std::bad_alloc) {
   ++number_of_allocs;
   total_allocated += size;
   void *p = malloc(size);
   if(!p) throw std::bad_alloc();
   return p;
}
void* operator new  [](std::size_t size) throw(std::bad_alloc) {
   ++number_of_allocs;
   total_allocated += size;
   void *p = malloc(size);
   if(!p) throw std::bad_alloc();
   return p;
}
void* operator new  [](std::size_t size, const std::nothrow_t&) throw() {
   ++number_of_allocs;
   total_allocated += size;
   return malloc(size);
}
void* operator new   (std::size_t size, const std::nothrow_t&) throw() {
   ++number_of_allocs;
   total_allocated += size;
   return malloc(size);
}
void operator delete(void* ptr) throw() {
   ++number_of_frees;
   free(ptr);
}
void operator delete (void* ptr, const std::nothrow_t&) throw() {
   ++number_of_frees;
   free(ptr);
}
void operator delete[](void* ptr) throw() {
   ++number_of_frees;
   free(ptr);
}
void operator delete[](void* ptr, const std::nothrow_t&) throw() {
   ++number_of_frees;
   free(ptr);
}

void mem_end_test(void* output) {
   end_test (output, total_allocated, number_of_allocs, number_of_frees);
}
void mem_start_test(void* output, const char* name) {
   start_test(output, name);
}

/**
 * test_cpp_int_list
 */

extern "C"
void test_cpp_int_list (void * output) {
   reset_mem();

   start_container_test (output, "C++", "", "", "Integer List", 1);
   save_container_size (output, sizeof(std::list<int>));

   for (int r = 0; r < repeat_count; r++) {
      std::list<int>  v;

      mem_start_test (output, "fill");
      for (int c = 1; c <= items_count; c++) {
	 v.push_back(c);
      }
      mem_end_test (output);

      mem_start_test (output, "copy");
      {
	 std::list<int> v_copy (v);
	 mem_end_test (output);
      }

      int count = 0;
      mem_start_test (output, "cursor loop");
      for (auto it = v.begin(), __end=v.end(); it != __end; ++it) {
	 if (*it <= 2) {
	    count ++;
	 }
      }
      mem_end_test (output);
      if (count != 2) {
	 std::cout << "C++ error while counting" << std::endl;
      }

      count = 0;
      mem_start_test (output, "for-of loop");
      for (auto e : v) {
	 if (e <= 2) {
	    count ++;
	 }
      }
      mem_end_test (output);
      if (count != 2) {
	 std::cout << "C++ error while counting" << std::endl;
      }

      mem_start_test (output, "count_if");
      count = std::count_if (v.begin(), v.end(), IsLessEqual2);
      mem_end_test (output);
      if (count != 2) {
	 std::cout << "C++ error while counting" << std::endl;
      }

      /*
      mem_start_test (output, "find_if");
      auto found = std::find_if (v.begin(), v.end(), IsEqualItemsCount);
      mem_end_test (output);
      if (found == v.end()) {
	 std::cout << "C++ error while searching" << std::endl;
      }
      */
   }

   end_container_test
      (output, total_allocated, number_of_allocs, number_of_frees);
}

/**
 * test_cpp_str_list
 */


extern "C"
void test_cpp_str_list (void * output) {
   reset_mem();

   start_container_test (output, "C++", "", "", "String List", 1);
   save_container_size (output, sizeof(std::list<int>));

   for (int r = 0; r < repeat_count; r++) {
      std::list<std::string>  v;

      mem_start_test (output, "fill");
      for (int c = 1; c <= items_count; c++) {
         if (c % 2 == 0) {
            v.push_back("foo");
         } else {
            v.push_back("foofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoofoo");
         }
      }
      mem_end_test (output);

      mem_start_test (output, "copy");
      {
	 std::list<std::string> v_copy (v);
	 mem_end_test (output);
      }

      int count = 0;
      mem_start_test (output, "cursor loop");
      for (auto it = v.begin(), __end=v.end(); it != __end; ++it) {
	 if (startsWithStr(*it)) {
	    count ++;
	 }
      }
      mem_end_test (output);
      if (count != items_count) {
	 std::cout << "C++ error while counting" << std::endl;
      }

      count = 0;
      mem_start_test (output, "for-of loop");
      for (std::string& e : v) {
	 if (startsWithStr(e)) {
	    count ++;
	 }
      }
      mem_end_test (output);
      if (count != items_count) {
	 std::cout << "C++ error while counting" << std::endl;
      }

      mem_start_test (output, "count_if");
      count = std::count_if (v.begin(), v.end(), startsWithStr);
      mem_end_test (output);
      if (count != items_count) {
	 std::cout << "C++ error while counting" << std::endl;
      }

      /*
      mem_start_test (output, "find_if");
      auto found = std::find_if (v.begin(), v.end(), startsWithB);
      mem_end_test (output);
      if (found != v.end()) {
	 std::cout << "C++ error while searching" << std::endl;
      }
      */
   }

   end_container_test
      (output, total_allocated, number_of_allocs, number_of_frees);
}

/**
 * test_cpp_str_str_map
 */

extern "C"
void test_cpp_str_str_map (void * output) {
   typedef std::map<std::string, std::string> str_str_map;

   reset_mem();

   start_container_test
      (output, "C++", "ordered", "", "StrStr Map", 1);
   save_container_size (output, sizeof(str_str_map));

   for (int r = 0; r < repeat_count; r++) {
      str_str_map v;

      mem_start_test (output, "fill");
      for (int c = 1; c <= items_count; c++) {
	 v[std::to_string(c)] = "foo";
      }
      mem_end_test (output);

      mem_start_test (output, "copy");
      {
	 str_str_map v_copy (v);
	 mem_end_test (output);
      }

      int count = 0;
      mem_start_test (output, "cursor loop");
      for (auto it = v.begin(), __end=v.end(); it != __end; ++it) {
	 //  ??? Using valueStartsWithStr(*it) is twice as slow...
	 if (startsWithStr(it->second)) {  // value
	    count ++;
	 }
      }
      mem_end_test (output);
      if (count != items_count) {
	 std::cout << "C++ error while counting" << count << std::endl;
      }

      count = 0;
      mem_start_test (output, "for-of loop");
      for (auto& e : v) {
	 if (startsWithStr(e.second)) { // value
	    count ++;
	 }
      }
      mem_end_test (output);
      if (count != items_count) {
	 std::cout << "C++ error while counting" << count << std::endl;
      }

      mem_start_test (output, "count_if");
      count = std::count_if (v.begin(), v.end(), valueStartsWithStr);
      mem_end_test (output);
      if (count != items_count) {
	 std::cout << "C++ error while counting" << std::endl;
      }

      mem_start_test (output, "find");
      for (int c = 1; c <= items_count; c++) {
	 auto found = v["1"];
      }
      mem_end_test (output);
   }

   end_container_test
      (output, total_allocated, number_of_allocs, number_of_frees);
}

/**
 * test_cpp_str_str_unordered_map
 */

extern "C"
void test_cpp_str_str_unordered_map (void * output) {
   typedef std::unordered_map<std::string, std::string> str_str_unordered_map;

   reset_mem();

   start_container_test
      (output, "C++", "unordered", "", "StrStr Map", 1);
   save_container_size (output, sizeof(str_str_unordered_map));

   for (int r = 0; r < repeat_count; r++) {
      str_str_unordered_map v;

      mem_start_test (output, "fill");
      for (int c = 1; c <= items_count; c++) {
	      v[std::to_string(c)] = "foo";
      }
      mem_end_test (output);

      mem_start_test (output, "copy");
      {
	      str_str_unordered_map v_copy (v);
	      mem_end_test (output);
      }

      int count = 0;
      mem_start_test (output, "cursor loop");
      for (auto it = v.begin(), __end=v.end(); it != __end; ++it) {
         //  ??? Using valueStartsWithStr(*it) is twice as slow...
         if (startsWithStr(it->second)) {  // value
            count ++;
         }
      }
      mem_end_test (output);
      if (count != items_count) {
         std::cout << "C++ error while counting" << count << std::endl;
      }

      count = 0;
      mem_start_test (output, "for-of loop");
      for (auto& e : v) {
         if (startsWithStr(e.second)) { // value
            count ++;
         }
      }
      mem_end_test (output);
      if (count != items_count) {
         std::cout << "C++ error while counting" << count << std::endl;
      }

      mem_start_test (output, "count_if");
      count = std::count_if (v.begin(), v.end(), valueStartsWithStr);
      mem_end_test (output);
      if (count != items_count) {
         std::cout << "C++ error while counting" << std::endl;
      }

      count = 0;
      mem_start_test (output, "find");
      for (int c = 1; c <= items_count; c++) {
         if (startsWithStr(v[" 1"])) {
            count++;
         }
      }
      mem_end_test (output);
   }

   end_container_test
      (output, total_allocated, number_of_allocs, number_of_frees);
}
