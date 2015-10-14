------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;
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
