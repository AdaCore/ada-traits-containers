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

with Ada.Text_IO; use Ada.Text_IO;

package body Conts.Dicts is

   package body Generic_Dicts is

      function Get (Self : Dict; Position : in out Cursor) return Slot;
      pragma Inline (Get);

      procedure Set (Self     : in out Dict;
                     Position : Cursor;
                     Value    : Slot);

      procedure Set
        (Self           : in out Dict;
         Position       : Cursor;
         Key            : Key_Type;
         Value          : Value_Type;
         Enable_Realloc : Boolean;
         Checksum       : Index_Type);

      function Find_Slot
        (Self     : Dict;
         Key      : Key_Type;
         Checksum : Index_Type)
        return Cursor;

      function Realloc
        (Self : in out Dict;
         Size : Index_Type)
         return Boolean;
      --  Reallocate a dictionary in order to expand or shrink the container

      procedure Allocate_Internal (Self : in out Dict;
                                   Table_Size : Index_Type);

      procedure Release is
        new Ada.Unchecked_Deallocation (Slot_Table, Slot_Table_Access);

      --------------
      -- Allocate --
      --------------

      procedure Allocate (Self : in out Dict; Min_Size : Index_Type := 0) is
         Table_Size : Index_Type := Container_Size (0, Min_Size);
      begin
         Allocate_Internal (Self, Table_Size - 1);
      end Allocate;

      procedure Allocate_Internal (Self : in out Dict;
                                   Table_Size : Index_Type)
      is
      begin
         if Table_Size + 1 <= In_Place_Table_Limit then
            declare
               T : Slot_Table (0 .. In_Place_Table_Limit - 1) :=
                 (others => Empty_Slot);
            begin
               Self := (Short_Dict, 0, 0, In_Place_Table_Limit - 1, T);
            end;
         else
            declare
               T : constant Slot_Table_Access :=
                 new Slot_Table'(0 .. Table_Size => Empty_Slot);
            begin
               Self := (Long_Dict, 0, 0, Table_Size, T);
            end;
         end if;
      end Allocate_Internal;

      --------------
      -- Contains --
      --------------

      function Contains (Self : Dict; Key : Key_Type) return Boolean
      is
         Position : Cursor := Find_Slot (Self, Key, Hash (Key));
      begin
         return Position.Kind = Active;
      end Contains;

      ----------
      -- Copy --
      ----------

      function Copy (Self : Dict) return Dict
      is
      begin
         return Self;
      end Copy;

      ------------
      -- Delete --
      ------------

      procedure Delete
         (Self        : in out Dict;
          Key         : Key_Type;
          Freeze_Size : Boolean := False)
      is
         Position : Cursor := Element (Self, Key);
      begin
         case Position.Kind is
            when Empty =>
               return;
            when Dummy =>
               return;
            when Active =>
               declare
                  Value : Slot := Get (Self, Position);
               begin
                  Values.Release (Value.Value);
                  Keys.Release (Value.Key);
                  Set (Self, Position, Dummy_Slot);
                  Self.Active := Self.Active - 1;
               end;
         end case;

         if not Freeze_Size then
            declare
               Size_Changed : Boolean;
            begin
               Size_Changed := Realloc (Self, Self.Active);
            end;
         end if;
      end Delete;

      -------------
      -- Element --
      -------------

      function Element (Self : Dict; Key : Key_Type) return Value_Type
      is
         Position : Cursor := Element (Self, Key);
      begin
         if Position.Kind /= Active then
            raise Key_Error;
         end if;

         return Values.Convert_To (Get (Self, Position).Value);
      end Element;

      -------------
      -- Element --
      -------------

      function Element (Self : Dict; Key : Key_Type) return Cursor
      is
      begin
         return Find_Slot (Self, Key, Hash (Key));
      end Element;

      ---------
      -- Get --
      ---------

      function Get (Self : Dict; Position : in out Cursor) return Slot
      is
         Result : Slot;
      begin
         case Self.Kind is
            when Short_Dict =>
               Result := Self.Short_Table (Position.Index);
            when Long_Dict =>
               Result := Self.Long_Table (Position.Index);
         end case;
         Position.Kind := Result.Kind;
         if Result.Kind = Active then
            Position.Hash := Result.Hash;
         else
            Position.Hash := 0;
         end if;
         return Result;
      end Get;

      ---------------
      -- Find_Slot --
      ---------------

      function Find_Slot
        (Self      : Dict;
         Key       : Key_Type;
         Checksum  : Index_Type)
         return Cursor
      is
         Short_Hash   : Index_Type := Self.Mask and Checksum;
         Perturb      : Index_Type := Checksum;
         Dummy_Cursor : Cursor := Null_Cursor;
         Result       : Cursor := (Empty, Short_Hash, Checksum);
      begin
         loop
            declare
               Value : constant Slot := Get (Self, Result);
            begin
               case Result.Kind is
                  when Empty =>
                     exit;

                  when Dummy =>
                     --  In case of dummy entry we need to follow the search,
                     --  but keep track of the first dummy entry in a sequence
                     --  of dummy entries
                     if Dummy_Cursor /= Null_Cursor then
                        Dummy_Cursor := Result;
                     end if;

                  when Active =>
                     Dummy_Cursor := Null_Cursor;

                     if Value.Hash = Checksum and then
                       Equals (Keys.Convert_To (Value.Key), Key)
                     then
                        exit;
                     end if;
               end case;

               Result.Index := (Result.Index * 5 + 1 + Perturb) and Self.Mask;
               Perturb := Perturb / 2 ** 5;
            end;
         end loop;

         if Dummy_Cursor /= Null_Cursor then
            return Dummy_Cursor;
         else
            return Result;
         end if;
      end Find_Slot;

      -----------
      -- First --
      -----------

      function First (Self : Dict) return Cursor
      is
         Result : Cursor := Null_Cursor;
      begin
         if Self.Active > 0 then
            for Index in 0 .. Self.Mask loop
               Result.Index := Index;
               exit when Get (Self, Result).Kind = Active;
            end loop;
         end if;
         return Result;
      end First;

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element (Self : Dict; Position : Cursor) return Boolean is
         pragma Unreferenced (Self);
      begin
         return Position.Kind = Active;
      end Has_Element;

      ---------
      -- Key --
      ---------

      function Key (Self : Dict; Position : Cursor) return Key_Type
      is
         Tmp : Cursor := Position;
         Result : Slot;
      begin
         Result := Get (Self, Tmp);
         if Tmp.Kind /= Active then
            raise Key_Error;
         end if;
         return Keys.Convert_To (Result.Key);
      end Key;

      ----------
      -- Next --
      ----------

      function Next (Self : Dict; Position : Cursor) return Cursor
      is
         Result : Cursor := Null_Cursor;
      begin
         for Index in Position.Index + 1 .. Self.Mask loop
            Result.Index := Index;
            exit when Get (Self, Result).Kind = Active;
         end loop;
         return Result;
      end Next;

      -------------
      -- Realloc --
      -------------

      function Realloc
        (Self : in out Dict;
         Size : Index_Type)
         return Boolean
      is
         New_Mask : Index_Type := Container_Size (Self.Mask + 1, Size) - 1;
      begin
         --  First check if we the number of active elements is > to 2/3 of the
         --  table size. If this is the case then reallocate the table by
         --  multiplying its size by 4
         if New_Mask /= Self.Mask then
            declare
               New_Dict  : Dict;
            begin
               Allocate_Internal (New_Dict, New_Mask);

               for Index in 0 .. Self.Mask loop
                  declare
                     Previous_Position : Cursor := (Empty, Index, 0);
                     Value    : constant Slot := Get (Self, Previous_Position);
                     Position : Cursor;
                  begin
                     if Value.Kind = Active then
                        Position := Find_Slot
                          (New_Dict,
                           Keys.Convert_To (Value.Key),
                           Value.Hash);
                        Set (New_Dict, Position, Value);
                     end if;
                  end;
               end loop;
               New_Dict.Active := Self.Active;
               New_Dict.Fill   := Self.Active;

               if Self.Kind = Long_Dict then
                  Release (Self.Long_Table);
               end if;
               Self := New_Dict;
            end;
            return True;
         end if;
         return False;
      end Realloc;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Dict) is
      begin
         --  Release key and values
         for Index in 0 .. Self.Mask loop
            declare
               Position : Cursor := (Empty, Index, 0);
               Value    : Slot := Get (Self, Position);
            begin
               if Value.Kind = Active then
                  Keys.Release (Value.Key);
                  Values.Release (Value.Value);
               end if;
            end;
         end loop;

         --  Release if long table
         if Self.Kind = Long_Dict then
            Release (Self.Long_Table);
         end if;

      end Release;

      ---------
      -- Set --
      ---------

      procedure Set
        (Self           : in out Dict;
         Key            : Key_Type;
         Value          : Value_Type;
         Enable_Realloc : Boolean := True)
      is
         Checksum : Index_Type := Hash (Key);
      begin
         Set (Self,
              Find_Slot (Self, Key, Checksum),
              Key,
              Value,
              Enable_Realloc,
              Checksum);
      end Set;

      procedure Set (Self     : in out Dict;
                     Position : Cursor;
                     Key      : Key_Type;
                     Value    : Value_Type;
                     Enable_Realloc : Boolean := True)
      is
         H : Index_Type := Position.Hash;
      begin
         if Position.Kind /= Active then
            H := Hash (Key);
         end if;

         Set (Self, Position,
              Key,
              Value,
              Enable_Realloc,
              H);
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set
        (Self           : in out Dict;
         Position       : Cursor;
         Key            : Key_Type;
         Value          : Value_Type;
         Enable_Realloc : Boolean;
         Checksum       : Index_Type)
      is
         Size_Changed : Boolean := False;
         Current_Pos  : Cursor := Position;
         Current_Slot : Slot := Get (Self, Current_Pos);
         New_Slot     : Slot;
         New_Position : Cursor := Position;
      begin
         case Current_Slot.Kind is
            when Empty | Dummy =>
               --  check is reallocation is needed
               if Enable_Realloc then
                  Size_Changed := Realloc (Self, Self.Active + 1);
               end if;

               if Current_Slot.Kind = Empty then
                  Self.Fill := Self.Fill + 1;
               end if;

               Self.Active := Self.Active + 1;

               New_Slot := (Active,
                            Checksum,
                            Keys.Convert_From (Key),
                            Values.Convert_From (Value));

               if Size_Changed then
                  New_Position := Find_Slot (Self, Key, New_Slot.Hash);
               end if;

            when Active =>
               New_Slot := Current_Slot;
               Values.Release (New_Slot.Value);
               New_Slot.Value := Values.Convert_From (Value);
         end case;

         Set (Self, New_Position, New_Slot);
      end Set;

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Dict; Position : Cursor; Value : Slot) is
      begin
         case Self.Kind is
            when Short_Dict =>
               Self.Short_Table (Position.Index) := Value;
            when Long_Dict =>
               Self.Long_Table (Position.Index) := Value;
         end case;
      end Set;

      ----------
      -- Size --
      ----------

      function Size (Self : Dict) return Index_Type
      is
      begin
         return Self.Active;
      end Size;

      -----------
      -- Value --
      -----------

      function Value (Self : Dict; Position : Cursor) return Value_Type
      is
         Tmp : Cursor := Position;
         Result : Slot;
      begin
         Result := Get (Self, Tmp);
         if Tmp.Kind /= Active then
            raise Key_Error;
         end if;
         return Values.Convert_To (Result.Value);
      end Value;

   end Generic_Dicts;

   --------------------
   -- Container_Size --
   --------------------

   function Dynamic_Container_Size
     (Current_Size, Number_Of_Elements : Unsigned_32) return Unsigned_32
   is
      Result : Unsigned_32 := Current_Size;
   begin
      if Result = 0 then
         Result := 2;
      end if;

      loop
         if Number_Of_Elements * 3 > 2 * Result then
            Result := Result * 4;
         elsif Number_Of_Elements * 3 * 4 < Result then
            Result := Result / 4;
         else
            exit;
         end if;
      end loop;
      return Result;
   end Dynamic_Container_Size;

end Conts.Dicts;
