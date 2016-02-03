------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
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
with Ada.Containers; use Ada.Containers;

package body Conts.Maps is

   Min_Size : constant Hash_Type := 2 ** 3;
   --  Minimum size for maps. Must be a power of 2.

   package body Maps is

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Slot_Table, Slot_Table_Access);

      function Find_Slot
        (Self  : Map'Class;
         Key   : Keys.Element_Type;
         H     : Hash_Type) return Hash_Type;
      --  Probe the table and look for the place where the element with that
      --  key would be inserted (or already exists).

      procedure Internal_Insert
        (Self  : in out Map'Class;
         Key   : Keys.Element_Type;
         H     : Hash_Type;
         Value : Elements.Element_Type);
      --  Internal insert function.

      ---------------
      -- Find_Slot --
      ---------------

      function Find_Slot
        (Self  : Map'Class;
         Key   : Keys.Element_Type;
         H     : Hash_Type) return Hash_Type
      is
         Candidate   : Hash_Type := H and Self.Table'Last;
         First_Dummy : Hash_Type := Hash_Type'Last;
         Pertub      : Hash_Type := H;
         S           : Slot;
      begin
         loop
            S := Self.Table (Candidate);
            case S.Kind is
               when Empty =>
                  exit;

               when Dummy =>
                  --  In case of dummy entry we need to follow the search,
                  --  but keep track of the first dummy entry in a sequence
                  --  of dummy entries
                  if First_Dummy = Hash_Type'Last then
                     First_Dummy := Candidate;
                  end if;

               when Full =>
                  exit when S.Hash = H and then "=" (Key, S.Key);
            end case;

            --  We use open addressing, so we need to search for the next
            --  candidate slot. Linear probing (moving to the next slot) is
            --  not really efficient when possible use integers are keys in
            --  the table. So we use a more complicated pattern:

            Candidate :=
              (Candidate * 4 + Candidate + 1 + Pertub) and Self.Table'Last;
            Pertub := Pertub / (2 ** 5);
         end loop;

         if First_Dummy /= Hash_Type'Last then
            return First_Dummy;
         else
            return Candidate;
         end if;
      end Find_Slot;

      ---------------------
      -- Internal_Insert --
      ---------------------

      procedure Internal_Insert
        (Self  : in out Map'Class;
         Key   : Keys.Element_Type;
         H     : Hash_Type;
         Value : Elements.Element_Type)
      is
         Index : constant Hash_Type := Find_Slot (Self, Key, H);
         S     : Slot renames Self.Table (Index);
         Old_Kind : constant Slot_Kind := S.Kind;
      begin
         case Old_Kind is
            when Empty =>
               S := (Hash  => H,
                     Kind  => Full,
                     Key   => Keys.To_Stored (Key),
                     Value => Elements.To_Stored (Value));
               Self.Used := Self.Used + 1;
               Self.Fill := Self.Fill + 1;

            when Dummy =>
               S := (Hash  => H,
                     Kind  => Full,
                     Key   => Keys.To_Stored (Key),
                     Value => Elements.To_Stored (Value));
               Self.Used := Self.Used + 1;

            when Full =>
               Elements.Release (S.Value);
               S.Value := Elements.To_Stored (Value);
         end case;
      end Internal_Insert;

      --------------
      -- Capacity --
      --------------

      function Capacity (Self : Map'Class) return Count_Type is
      begin
         if Self.Table = null then
            return 0;
         else
            return Self.Table'Length;
         end if;
      end Capacity;

      ------------
      -- Resize --
      ------------

      procedure Resize
        (Self     : in out Map'Class;
         New_Size : Hash_Type)
      is
         Size      : Hash_Type := Min_Size;
         Tmp       : Slot_Table_Access;
         Candidate : Hash_Type;
         Pertub    : Hash_Type;
      begin
         --  Find smallest valid size greater than New_Size

         while Size < New_Size loop
            Size := Size * 2;
         end loop;

         Tmp := Self.Table;
         Self.Table := new Slot_Table (0 .. Size - 1);

         --  Reinsert all the elements in the new table. We do not need to
         --  recompute their hashes, which are unchanged an cached. Since we
         --  know there are no duplicate keys either, we can simplify the
         --  search for the slot, in particular no need to compare the keys.
         --  There are also no dummy slots

         if Tmp /= null then
            for E in Tmp'Range loop
               if Tmp (E).Kind = Full then
                  Pertub := Tmp (E).Hash;
                  Candidate := Pertub and Self.Table'Last;
                  loop
                     if Self.Table (Candidate).Kind = Empty then
                        Self.Table (Candidate) :=
                          (Hash  => Tmp (E).Hash,
                           Kind  => Full,
                           Key   => Tmp (E).Key,
                           Value => Tmp (E).Value);
                        exit;
                     end if;

                     Candidate :=
                       (Candidate * 4 + Candidate + 1 + Pertub)
                       and Self.Table'Last;
                     Pertub := Pertub / (2 ** 5);
                  end loop;
               end if;
            end loop;

            Unchecked_Free (Tmp);
         end if;
      end Resize;

      ---------
      -- Set --
      ---------

      procedure Set
        (Self     : in out Map'Class;
         Key      : Keys.Element_Type;
         Value    : Elements.Element_Type)
      is
         H    : constant Hash_Type := Hash (Key);
         Used : constant Hash_Type := Self.Used;
         New_Size : Hash_Type;
      begin
         --  At least one empty slot
         pragma Assert (Self.Fill <= Self.Capacity);

         if Self.Table = null then
            Resize (Self, Min_Size);
         end if;

         Internal_Insert (Self, Key, H, Value);

         --  We might need to resize if we just added a key

         if Self.Used > Used
           and then Self.Fill * 3 >= Self.Capacity * 2
         then
            --  Quadruple the size in general which improves sparseness and
            --  reduces the number of resize operations, which are costly.
            --  However, large dictionaries are only doubled

            if Self.Used > 100_000 then
               New_Size := Self.Used * 2;
            else
               New_Size := Self.Used * 4;
            end if;

            Resize (Self, New_Size);
         end if;
      end Set;

      ---------
      -- Get --
      ---------

      function Get
        (Self     : Map'Class;
         Key      : Keys.Element_Type)
         return Elements.Return_Type is
      begin
         if Self.Table /= null then
            declare
               H     : constant Hash_Type := Hash (Key);
               Index : constant Hash_Type := Find_Slot (Self, Key, H);
            begin
               if Self.Table (Index).Kind = Full then
                  return Elements.To_Return (Self.Table (Index).Value);
               end if;
            end;
         end if;
         raise Constraint_Error with "Key not in map";
      end Get;

      ------------
      -- Delete --
      ------------

      procedure Delete
        (Self     : in out Map'Class;
         Key      : Keys.Element_Type) is
      begin
         if Self.Table /= null then
            declare
               H     : constant Hash_Type := Hash (Key);
               Index : constant Hash_Type := Find_Slot (Self, Key, H);
               S     : Slot renames Self.Table (Index);
            begin
               if S.Kind = Full then
                  Keys.Release (S.Key);
                  Elements.Release (S.Value);
                  S.Kind := Dummy;
                  Self.Used := Self.Used - 1;
                  --   unchanged: Self.Fill
               end if;
            end;
         end if;
      end Delete;

      -----------
      -- Clear --
      -----------

      procedure Clear (Self : in out Map'Class) is
      begin
         if Self.Table /= null then
            for S of Self.Table.all loop
               if S.Kind = Full then
                  Keys.Release (S.Key);
                  Elements.Release (S.Value);
               end if;
            end loop;
            Unchecked_Free (Self.Table);
            Self.Used := 0;
            Self.Fill := 0;
         end if;
      end Clear;

      ------------
      -- Adjust --
      ------------

      procedure Adjust (Self : in out Map) is
         Tmp : constant Slot_Table_Access := Self.Table;
      begin
         if Tmp /= null then

            if Elements.Copyable and then Keys.Copyable then
               Self.Table := new Slot_Table'(Tmp.all);
            else
               Self.Table := new Slot_Table (Tmp'Range);
               for E in Self.Table'Range loop
                  if Tmp (E).Kind = Full then
                     Self.Table (E) :=
                       (Hash  => Tmp (E).Hash,
                        Kind  => Full,
                        Key   =>
                          (if Keys.Copyable
                           then Tmp (E).Key
                           else Keys.Copy (Tmp (E).Key)),
                        Value =>
                          (if Elements.Copyable
                           then Tmp (E).Value
                           else Elements.Copy (Tmp (E).Value)));
                  else
                     Self.Table (E) := Tmp (E);
                  end if;
               end loop;
            end if;
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Self : in out Map) is
      begin
         Clear (Self);
      end Finalize;

   end Maps;

end Conts.Maps;
