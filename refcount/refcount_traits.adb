with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;

package body Refcount_Traits is

   package body Smart_Pointers is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Object_Refcount, Object_Refcount_Access);

      use Elements;

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref'Class; Data : Element_Type) is
         Tmp : constant Stored_Element_Type := Convert_From (Data);
      begin
         Self.Adopt (Tmp);
      end Set;

      -----------
      -- Adopt --
      -----------

      procedure Adopt (Self : in out Ref; Data : Stored_Element_Type) is
      begin
         if Self.Data /= null
            and then Self.Data.Object = Stored_Element_Type (Data)
         then
            return;  --  Avoid finalizing if we are going to reuse it
         end if;

         --  ??? Possible error if Data comes from another smart pointer,
         --  since they will both share the same data.
         Finalize (Self);
         Self.Data := new Object_Refcount'
             (Refcount  => 1,
              Object    => Data);
      end Adopt;

      ---------
      -- Get --
      ---------

      function Get (Self : Ref'Class) return Stored_Element_Type is
      begin
         --  ??? Fails if Self is Null_Ref, but not clear what to return
         --  in this case. Perhaps simply using a predcondition ?

         --  if Self.Data = null then
         --     return null;
         --  else
            return Self.Data.Object;
         --  end if;
      end Get;

      ---------------
      -- Reference --
      ---------------

      function Reference (Self : Ref'Class) return Reference_Type is
      begin
         --  Fails if Self.Data is null
         return Elements.Get_Reference (Self.Data.Object);
      end Reference;

      ---------
      -- "=" --
      ---------

      overriding function "=" (P1, P2 : Ref) return Boolean is
      begin
         return P1.Data = P2.Data;
      end "=";

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Self : in out Ref) is
      begin
         if Self.Data /= null then
            if Thread_Safe then
               Sync_Add_And_Fetch (Self.Data.Refcount'Access, 1);
            else
               Self.Data.Refcount := Self.Data.Refcount + 1;
            end if;
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Ref) is
         Data : Object_Refcount_Access := Self.Data;
         Tmp  : Interfaces.Integer_32;
      begin
         Self.Data := null;   --  make idempotent
         if Data /= null then
            if Thread_Safe then
               Tmp := Sync_Add_And_Fetch (Data.Refcount'Access, -1);
            else
               Data.Refcount := Data.Refcount - 1;
               Tmp := Data.Refcount;
            end if;

            if Tmp = 0 then
               Release (Data.Object);
               Unchecked_Free (Data);
            end if;
         end if;
      end Finalize;

   end Smart_Pointers;

end Refcount_Traits;
