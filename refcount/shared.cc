#include <ctime>
#include <iostream>
#include <memory>

extern "C" {
   void test_shared_int(void* output);
   void test_shared_str(void* output);
   extern void ada_print_time(void* output, double elapsed);
   extern void ada_start_line(void* output, const char* title);
   extern const int items_count_for_smart_pointers;
};

void test_shared_int(void* output) {
   ada_start_line(output, "C++ int");

   // Set
   std::clock_t begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<int> ref (new int);
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   // Assign

   std::shared_ptr<int> ref (new int);
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<int> ref2 = ref;
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   // Get
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      volatile int val = *ref;
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
}

void test_shared_str(void* output) {
   ada_start_line(output, "C++ str");

   // Set
   std::clock_t begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<std::string> ref (new std::string("Foo"));
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   // Assign

   std::shared_ptr<std::string> ref (new std::string("Foo"));
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<std::string> ref2 = ref;
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);

   // Get
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      if (*ref != "Foo") {
         std::cout << "Error";
         break;
      }
   }
   ada_print_time(output, double(clock() - begin) / CLOCKS_PER_SEC);
}
