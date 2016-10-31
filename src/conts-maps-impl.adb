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

package body Conts.Maps.Impl with SPARK_Mode => Off is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   Min_Size : constant Count_Type := 2 ** 3;
   --  Minimum size for maps. Must be a power of 2.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Slot_Table, Slot_Table_Access);

   function Find_Slot
     (Self  : Base_Map'Class;
      Key   : Keys.Element_Type;
      H     : Hash_Type) return Hash_Type;
   --  Probe the table and look for the place where the element with that
   --  key would be inserted (or already exists).

   -----------
   -- Model --
   -----------

   function Model (Self : Base_Map'Class) return M.Map is
      R : M.Map;
      C : Cursor;
   begin
      --  If Self is empty, so is its model.

      if Self.Table = null then
         return R;
      end if;

      --  Search for the first non-empty slot in Self.

      C.Index := Self.Table'First;
      while C.Index <= Self.Table'Last
        and then Self.Table (C.Index).Kind /= Full
      loop
         C.Index := C.Index + 1;
      end loop;

      --  Loop over the content of Self.

      while C.Index <= Self.Table'Last loop
         pragma Assert (Self.Table (C.Index).Kind = Full);

         --  Store the current element in R.

         declare
            S : constant Slot := Self.Table (C.Index);
            K : constant Key_Type := Keys.To_Element
              (Keys.To_Constant_Returned (S.Key));
            V : constant Element_Type := Elements.To_Element
              (Elements.To_Constant_Returned (S.Value));
         begin
            R := M.Add (R, K, V);
         end;

         --  Go to the next non-empty slot.

         C := (Index => C.Index + 1);
         while C.Index <= Self.Table'Last
           and then Self.Table (C.Index).Kind /= Full
         loop
            C.Index := C.Index + 1;
         end loop;
      end loop;

      return R;
   end Model;

   ------------
   -- S_Keys --
   ------------

   function S_Keys (Self : Base_Map'Class) return K.Sequence is
      R : K.Sequence;
      C : Cursor;

   begin
      --  If Self is empty, so is its sequence of keys.

      if Self.Table = null then
         return R;
      end if;

      --  Search for the first non-empty slot in Self.

      C.Index := Self.Table'First;
      while C.Index <= Self.Table'Last
        and then Self.Table (C.Index).Kind /= Full
      loop
         C.Index := C.Index + 1;
      end loop;

      --  Loop over the content of Self.

      while C.Index <= Self.Table'Last loop
         pragma Assert (Self.Table (C.Index).Kind = Full);

         --  Store the current key in R.

         declare
            S : constant Slot := Self.Table (C.Index);
            L : constant Key_Type := Keys.To_Element
              (Keys.To_Constant_Returned (S.Key));
         begin
            R := K.Add (R, L);
         end;

         --  Go to the next non-empty slot.

         C := (Index => C.Index + 1);
         while C.Index <= Self.Table'Last
           and then Self.Table (C.Index).Kind /= Full
         loop
            C.Index := C.Index + 1;
         end loop;
      end loop;

      return R;
   end S_Keys;

   ---------------
   -- Positions --
   ---------------

   function Positions (Self : Base_Map'Class) return P_Map is
      R : P.Map;
      C : Cursor;
      I : Count_Type := 0;

   begin
      --  If Self is empty, so is its position map.

      if Self.Table = null then
         return (Content => R);
      end if;

      --  Search for the first non-empty slot in Self.

      C.Index := Self.Table'First;
      while C.Index <= Self.Table'Last
        and then Self.Table (C.Index).Kind /= Full
      loop
         C.Index := C.Index + 1;
      end loop;

      --  Loop over the content of Self.

      while C.Index <= Self.Table'Last loop
         pragma Assert (Self.Table (C.Index).Kind = Full);

         --  Store the current cursor in R at position I + 1.

         I := I + 1;
         R := P.Add (R, C, I);

         --  Go to the next non-empty slot.

         C := (Index => C.Index + 1);
         while C.Index <= Self.Table'Last
           and then Self.Table (C.Index).Kind /= Full
         loop
            C.Index := C.Index + 1;
         end loop;
      end loop;

      return (Content => R);
   end Positions;

   ----------------------------
   -- Lift_Abstraction_Level --
   ----------------------------

   procedure Lift_Abstraction_Level (Self : Base_Map'Class) is null;

   ---------------
   -- Find_Slot --
   ---------------

   function Find_Slot
     (Self  : Base_Map'Class;
      Key   : Keys.Element_Type;
      H     : Hash_Type) return Hash_Type
   is
      Candidate   : Hash_Type := H and Self.Table'Last;
      First_Dummy : Hash_Type := Hash_Type'Last;
      S           : Slot;
      Prob        : Probing;
   begin
      Prob.Initialize_Probing (Hash => H, Size => Self.Table'Last);

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

         Candidate := Prob.Next_Probing (Candidate) and Self.Table'Last;
      end loop;

      if First_Dummy /= Hash_Type'Last then
         return First_Dummy;
      else
         return Candidate;
      end if;
   end Find_Slot;

   ------------
   -- Assign --
   ------------

   procedure Assign (Self : in out Base_Map'Class; Source : Base_Map'Class) is
   begin
      Self.Used := Source.Used;
      Self.Fill := Source.Fill;
      Self.Table := Source.Table;
      Self.Adjust;
   end Assign;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Base_Map) is
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

   procedure Finalize (Self : in out Base_Map) is
   begin
      Clear (Self);
   end Finalize;

   -----------
   -- First --
   -----------

   function First (Self : Base_Map'Class) return Cursor is
      C : Cursor;
   begin
      if Self.Table = null then
         return No_Element;
      end if;

      C.Index := Self.Table'First;
      loop
         if C.Index > Self.Table'Last then
            return No_Element;
         end if;

         if Self.Table (C.Index).Kind = Full then
            return C;
         end if;

         C.Index := C.Index + 1;
      end loop;
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Base_Map'Class; Position : Cursor) return Boolean is
   begin
      return Position.Index <= Self.Table'Last;
   end Has_Element;

   ----------
   -- Next --
   ----------

   function Next
     (Self : Base_Map'Class; Position : Cursor) return Cursor
   is
      C : Cursor := (Index => Position.Index + 1);
   begin
      while C.Index <= Self.Table'Last
        and then Self.Table (C.Index).Kind /= Full
      loop
         C.Index := C.Index + 1;
      end loop;
      return C;
   end Next;

   ---------
   -- Key --
   ---------

   function Key
     (Self : Base_Map'Class; Position : Cursor)
         return Constant_Returned_Key_Type
   is
      P : Slot renames Self.Table (Position.Index);
   begin
      return Keys.To_Constant_Returned (P.Key);
   end Key;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Base_Map'Class; Position : Cursor)
         return Constant_Returned_Type
   is
      P : Slot renames Self.Table (Position.Index);
   begin
      return Elements.To_Constant_Returned (P.Value);
   end Element;

   --------------
   -- Capacity --
   --------------

   function Capacity (Self : Base_Map'Class) return Count_Type is
   begin
      if Self.Table = null then
         return 0;
      else
         return Self.Table'Length;
      end if;
   end Capacity;

   ------------
   -- Length --
   ------------

   function Length (Self : Base_Map'Class) return Count_Type is (Self.Used);

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Self     : in out Base_Map'Class;
      New_Size : Count_Type)
   is
      Size      : Hash_Type := Hash_Type (Min_Size);

      --  We need at least Length elements
      Min_New_Size : constant Hash_Type := Hash_Type'Max
         (Hash_Type (New_Size), Hash_Type (Self.Used));

      Tmp       : Slot_Table_Access;
      Candidate : Hash_Type;
      Prob      : Probing;
   begin
      --  Find smallest valid size greater than New_Size

      while Size < Min_New_Size loop
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
               Prob.Initialize_Probing
                 (Hash => Tmp (E).Hash, Size => Self.Table'Last);

               Candidate := Tmp (E).Hash and Self.Table'Last;
               loop
                  if Self.Table (Candidate).Kind = Empty then
                     Self.Table (Candidate) :=
                       (Hash  => Tmp (E).Hash,
                        Kind  => Full,
                        Key   => Tmp (E).Key,
                        Value => Tmp (E).Value);
                     exit;
                  end if;

                  Candidate := Prob.Next_Probing (Candidate)
                    and Self.Table'Last;
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
     (Self     : in out Base_Map'Class;
      Key      : Keys.Element_Type;
      Value    : Elements.Element_Type)
   is
      H        : constant Hash_Type := Hash (Key);
      Used     : constant Count_Type := Self.Used;
      New_Size : Count_Type;
   begin
      --  Need at least one empty slot
      pragma Assert (Self.Fill <= Self.Capacity);

      --  If the table was never allocated, do it now

      if Self.Table = null then
         Resize (Self, Min_Size);
      end if;

      --  Do the actual insert. Find_Slot expects to find an empy slot
      --  eventually, and the less full the table the more chance of
      --  finding this slot early on. But we can't systematically resize
      --  now, because replacing an element, for instance, doesn't need
      --  any resizing (so we would be wasting time or worse grow the table
      --  for nothing), nor does reusing a Dummy slot.
      --  So we really can only resize after the call to Find_Slot, which
      --  means we might be resizing even though the user won't be adding a
      --  new element ever after.

      declare
         Index    : constant Hash_Type := Find_Slot (Self, Key, H);
         S        : Slot renames Self.Table (Index);
      begin
         case S.Kind is
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
      end;

      --  If the table is now too full, we need to resize it for the next
      --  time we want to insert an element.

      if Self.Used > Used then
         New_Size := Resize_Strategy
           (Used     => Self.Used,
            Fill     => Self.Fill,
            Capacity => Self.Capacity);
         if New_Size /= 0 then
            Resize (Self, New_Size);
         end if;
      end if;
   end Set;

   ---------
   -- Get --
   ---------

   function Get
     (Self     : Base_Map'Class;
      Key      : Keys.Element_Type)
         return Elements.Constant_Returned_Type is
   begin
      if Self.Table /= null then
         declare
            H     : constant Hash_Type := Hash (Key);
            Index : constant Hash_Type := Find_Slot (Self, Key, H);
         begin
            if Self.Table (Index).Kind = Full then
               return Elements.To_Constant_Returned
                 (Self.Table (Index).Value);
            end if;
         end;
      end if;
      raise Constraint_Error with "Key not in map";
   end Get;

   --------------
   -- Contains --
   --------------

   function Contains (Self : Base_Map'Class; Key : Key_Type) return Boolean  is
   begin
      if Self.Table /= null then
         declare
            H     : constant Hash_Type := Hash (Key);
            Index : constant Hash_Type := Find_Slot (Self, Key, H);
         begin
            if Self.Table (Index).Kind = Full then
               return True;
            end if;
         end;
      end if;
      return False;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self     : in out Base_Map'Class;
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

   procedure Clear (Self : in out Base_Map'Class) is
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

end Conts.Maps.Impl;
