#include <ctime>
#include <memory>

extern "C" {
   void test_shared_int();
   void test_shared_str();
   extern void _ada_print_time(double elapsed);
   extern void _ada_start_line(const char* title);
   extern const int items_count_for_smart_pointers;
};

void test_shared_int() {
   _ada_start_line("C++ int");

   // Set
   std::clock_t begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<int> ref (new int);
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);

   // Assign

   std::shared_ptr<int> ref (new int);
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<int> ref2 = ref;
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);

   // Get
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      int val = *ref;
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
}

void test_shared_str() {
   _ada_start_line("C++ str");

   // Set
   std::clock_t begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<std::string> ref (new std::string("Foo"));
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);

   // Assign

   std::shared_ptr<std::string> ref (new std::string("Foo"));
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::shared_ptr<std::string> ref2 = ref;
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);

   // Get
   begin = clock();
   for (int i = 0; i < items_count_for_smart_pointers; i++) {
      std::string val = *ref;
   }
   _ada_print_time(double(clock() - begin) / CLOCKS_PER_SEC);
}
