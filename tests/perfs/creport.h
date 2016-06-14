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

#define START_GROUP 1
#define SAME_GROUP 0

extern "C" {
   extern const int items_count;
   extern const int repeat_count;
   extern void start_container_test
      (void* output, const char *name, const char* category, int favorite);
   extern void save_container_size (void* output, long int size);
   extern void start_test
      (void* output, const char* name, const int start_group);
   extern void end_test_not_run (void* output);
}

void reset_mem();
void* operator new(std::size_t size) throw(std::bad_alloc);
void* operator new  [](std::size_t size) throw(std::bad_alloc);
void* operator new  [](std::size_t size, const std::nothrow_t&) throw();
void* operator new   (std::size_t size, const std::nothrow_t&) throw();
void operator delete(void* ptr) throw();
void operator delete (void* ptr, const std::nothrow_t&) throw();
void operator delete[](void* ptr) throw();
void operator delete[](void* ptr, const std::nothrow_t&) throw();

void mem_end_test(void* output);
void mem_end_container_test(void* output);
