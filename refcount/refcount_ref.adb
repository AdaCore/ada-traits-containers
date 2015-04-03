with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;

package body Refcount_Ref is

   package body Smart_Pointers is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Interfaces.Integer_32, Object_Refcount);

      ---------
      -- Set --
      ---------

      function Set (Data : Element_Type) return Ref is
         Tmp : constant access Element_Type := new Element_Type'(Data);
      begin
         return R : Ref (E => Tmp) do
            R.Refcount := new Interfaces.Integer_32'(1);
         end return;
      end Set;

      ---------
      -- "=" --
      ---------

      overriding function "=" (P1, P2 : Ref) return Boolean is
      begin
         return P1.E = P2.E;
      end "=";

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Self : in out Ref) is
      begin
         if Self.Refcount /= null then
            if Thread_Safe then
               Sync_Add_And_Fetch (Self.Refcount, 1);
            else
               Self.Refcount.all := Self.Refcount.all + 1;
            end if;
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Ref) is
         Refcount : Object_Refcount := Self.Refcount;
         Tmp  : Interfaces.Integer_32;

         type Element_Access is access all Element_Type;
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
            (Element_Type, Element_Access);
         Ptr  : Element_Access;
      begin
         Self.Refcount := null;   --  make idempotent
         if Refcount /= null then
            if Thread_Safe then
               Tmp := Sync_Add_And_Fetch (Refcount, -1);
            else
               Refcount.all := Refcount.all - 1;
               Tmp := Refcount.all;
            end if;

            if Tmp = 0 then
               Free (Self.E.all);
               Ptr := Element_Access (Self.E);
               Unchecked_Free (Ptr);

               --  Self.E now points to freed memory. That is likely
               --  not an issue, since this was the last object
               --  pointing to that memory. Self could be finalized
               --  several times, but since Self.Refcount is now null,
               --  subsequent calls to Finalize have no effect.
               --  On the whole, we should be safe here.

               Unchecked_Free (Refcount);
            end if;
         end if;
      end Finalize;

   end Smart_Pointers;

end Refcount_Ref;
