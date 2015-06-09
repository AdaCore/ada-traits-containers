with Ada.Unchecked_Deallocation;

package body Conts.Dicts.Strings is

   procedure Deallocate is
     new Ada.Unchecked_Deallocation (String, String_Access);

   ------------------
   -- Convert_From --
   ------------------

   function Convert_From (E : String) return Stored_String
   is
   begin
      if E'Length <= Short_String_Size then
         declare
            S : String (1 .. Short_String_Size);
         begin
            S (1 .. E'Length) := E;
            return (Short_String, E, E'Length);
         end;
      else
         --  Put_Line ("allocate");
         return (Long_String, new String'(E));
      end if;
   end Convert_From;

   ----------------
   -- Convert_To --
   ----------------

   function Convert_To (E : Stored_String) return String
   is
   begin
      case E.Kind is
         when Short_String =>
            return E.Short_Value (1 .. E.Short_Length);
         when Long_String =>
            return E.Long_Value.all;
      end case;
   end Convert_To;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference (E : Stored_String) return String is
   begin
      return Convert_To (E);
   end Get_Reference;

   -------------
   -- Release --
   -------------

   procedure Release (E : in out Stored_String) is
   begin
      if E.Kind = Long_String then
         Deallocate (E.Long_Value);
      end if;
   end Release;

   ----------
   -- Hash --
   ----------

   function Hash (Str : String) return Unsigned_32 is
      Size : constant Natural := Str'Length;
      Result : Unsigned_32 := 0;
   begin
      if Size = 0 then
         return 0;
      end if;

      Result := Shift_Left (Unsigned_32 (Character'Pos (Str (Str'First))), 7);
      for Index in Str'First .. Str'Last loop

         Result := (1000003 * Result) xor
           Unsigned_32 (Character'Pos (Str (Index)));
      end loop;
      return Result;
   end Hash;

end Conts.Dicts.Strings;
