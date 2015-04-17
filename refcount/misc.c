
int ada_sync_bool_compare_and_swap(void** ptr, void* oldval, void* newval) {
   return __sync_bool_compare_and_swap(ptr, oldval, newval);
}
