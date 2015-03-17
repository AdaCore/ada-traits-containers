package body Perf_Support is

   -------------------
   -- Generic_Count --
   -------------------

   function Generic_Count
      (Start : Cursors.Cursor; F : Predicate.Obj'Class) return Natural
   is
      C : Natural := 0;
      S : Cursors.Cursor := Start;
   begin
      while Cursors.Has_Element (S) loop
         if F.Call (Cursors.Element (S)) then
            C := C + 1;
         end if;
         Cursors.Next (S);
      end loop;
      return C;
   end Generic_Count;

   -----------
   -- Count --
   -----------

   function Count
      (L : Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural
   is
      use Integer_Lists;
      C : Natural := 0;
      S : Integer_Lists.Cursor := L.First;
   begin
      while Has_Element (S) loop
         if Predicate (Element (S)) then
            C := C + 1;
         end if;
         Next (S);
      end loop;
      return C;
   end Count;

   -----------------------------
   -- Count_With_Generic_Func --
   -----------------------------

   function Count_With_Generic_Func
      (L : Mylists.My_Integer_Lists.List) return Natural
   is
      use Mylists.My_Integer_Lists;
      C : Natural := 0;
      S : Cursor := L.First;
   begin
      while Has_Element (S) loop
         if Predicate (Element (S)) then
            C := C + 1;
         end if;
         Next (S);
      end loop;
      return C;
   end Count_With_Generic_Func;

   ----------------------
   -- Count_With_Equal --
   ----------------------

   function Count_With_Equal
      (L : Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural
   is
      use Integer_Lists;
      C : Natural := 0;
      S : Integer_Lists.Cursor := L.First;
   begin
      while S /= No_Element loop
         if Predicate (Element (S)) then
            C := C + 1;
         end if;
         Next (S);
      end loop;
      return C;
   end Count_With_Equal;

   ------------------------
   -- Count_Separate_Pkg --
   ------------------------

   function Count_Separate_Pkg
      (L : Mylists.My_Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural
   is
      use Mylists.My_Integer_Lists;
      C : Natural := 0;
      S : Cursor := L.First;
   begin
      while Has_Element (S) loop
         if Predicate (Element (S)) then
            C := C + 1;
         end if;
         Next (S);
      end loop;
      return C;
   end Count_Separate_Pkg;

   -------------------------
   -- Count_With_Iterator --
   -------------------------

   function Count_With_Iterator
      (L : Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural
   is
      use Integer_Lists;
      C : Natural := 0;
   begin
      for Int of L loop
         if Predicate (Int) then
            C := C + 1;
         end if;
      end loop;
      return C;
   end Count_With_Iterator;

   --------------------------
   -- Containers_Hierarchy --
   --------------------------

   package body Containers_Hierarchy is
      overriding function First (C : List) return Forward_Cursor'Class is
      begin
         return List_Cursor'(C => C.L.First);
      end First;

      overriding procedure Append (C : in out List; P : T) is
      begin
         Internal_Lists.Append (C.L, P);
      end Append;

      overriding function Element (C : List_Cursor) return T is
      begin
         return Internal_Lists.Element (C.C);
      end Element;

      overriding procedure Next (C : in out List_Cursor) is
      begin
         Internal_Lists.Next (C.C);
      end Next;

      overriding function Has_Element (C : List_Cursor) return Boolean is
      begin
         return Internal_Lists.Has_Element (C.C);
      end Has_Element;
   end Containers_Hierarchy;

   ---------------------------
   -- Generic_Count_Virtual --
   ---------------------------

   function Generic_Count_Virtual
      (C : Containers.Forward_Cursor'Class) return Natural
   is
      Result : Natural := 0;
      C2 : Containers.Forward_Cursor'Class := C;
   begin
      while C2.Has_Element loop
         if Predicate (C2.Element) then
            Result := Result + 1;
         end if;
         C2.Next;
      end loop;
      return Result;
   end Generic_Count_Virtual;

   ------------------------
   -- Count_With_Functor --
   ------------------------

   function Count_With_Functor
      (L : Integer_Lists.List; F : Predicate.Obj'Class) return Natural
   is
      use Integer_Lists;
      C : Natural := 0;
      S : Integer_Lists.Cursor := L.First;
   begin
      while S /= No_Element loop
         if F.Call (Element (S)) then
            C := C + 1;
         end if;
         Next (S);
      end loop;
      return C;
   end Count_With_Functor;

end Perf_Support;
