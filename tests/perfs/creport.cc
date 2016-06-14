/****************************************************************************
 *                     Copyright (C) 2015-2016, AdaCore                     *
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

#include <iostream>

#define START_GROUP 1
#define SAME_GROUP 0

extern "C" {
   extern void end_container_test
      (void* output, int allocated, int allocs_count, int frees_count);
   extern void end_test
      (void* output, int allocated, int allocs_count, int frees_count);
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

void mem_end_container_test(void* output) {
   end_container_test
      (output, total_allocated, number_of_allocs, number_of_frees);
}
