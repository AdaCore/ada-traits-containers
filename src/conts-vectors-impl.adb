------------------------------------------------------------------------------
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body Conts.Vectors.Impl with SPARK_Mode => Off is
   use Conts.Vectors.Storage;

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   -----------
   -- Model --
   -----------

   function Model (Self : Base_Vector'Class) return M.Sequence is
      R  : M.Sequence;
   begin
      if Self.Last /= No_Last then
         for Idx in Min_Index .. Self.Last loop
            R := M.Add
               (R,
                Storage.Elements.To_Element
                   (Storage.Elements.To_Constant_Returned
                      (Storage.Get_Element (Self, Idx))));
         end loop;
      end if;
      return R;
   end Model;

   -----------
   -- First --
   -----------

   function First (Self : Base_Vector'Class) return Cursor is
   begin
      if Self.Last = No_Last then
         return No_Element;
      else
         return To_Index (Min_Index);
      end if;
   end First;

   ----------
   -- Next --
   ----------

   function Next
     (Self : Base_Vector'Class; Position : Cursor) return Cursor is
   begin
      if To_Count (Position) < Self.Last then
         return Cursor'Succ (Position);
      else
         return No_Element;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Self : Base_Vector'Class; Position : in out Cursor) is
   begin
      Position := Impl.Next (Self, Position);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Self : Base_Vector'Class; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Self);
   begin
      if To_Count (Position) > Min_Index then
         return Cursor'Pred (Position);
      else
         return No_Element;
      end if;
   end Previous;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Self : in out Base_Vector'Class; Capacity : Count_Type) is
   begin
      Storage.Resize
        (Self, Count_Type'Max (Self.Last, Capacity),
         Self.Last, Force => True);
   end Reserve_Capacity;

   -------------------
   -- Shrink_To_Fit --
   -------------------

   procedure Shrink_To_Fit (Self : in out Base_Vector'Class) is
   begin
      Storage.Resize (Self, Self.Last, Self.Last, Force => True);
   end Shrink_To_Fit;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Self    : in out Base_Vector'Class;
      Length  : Count_Type;
      Element : Storage.Elements.Element_Type)
   is
      Old_L : constant Count_Type := Self.Length;
   begin
      if Length < Old_L then
         for J in Length + 1 .. Old_L loop
            Storage.Release_Element (Self, J);
         end loop;
         Self.Last := Length;

      elsif Length > Old_L then
         Self.Append (Element, Count => Length - Old_L);
      end if;
   end Resize;

   ------------
   -- Length --
   ------------

   function Length (Self : Base_Vector'Class) return Count_Type is
   begin
      return Self.Last - Min_Index + 1;
   end Length;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self    : in out Base_Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
   is
      L : constant Count_Type := Self.Last;
   begin
      if L + Count > Self.Capacity then
         Storage.Resize (Self, L + Count, L, Force => False);
      end if;

      for J in 1 .. Count loop
         Storage.Set_Element
           (Self, L + J, Storage.Elements.To_Stored (Element));
      end loop;

      Self.Last := Self.Last + Count;
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self    : in out Base_Vector'Class;
      Before  : Extended_Index;
      Element : Element_Type;
      Count   : Count_Type := 1)
   is
   begin
      if Before = No_Element then
         Self.Append (Element, Count);
      else
         declare
            L : constant Count_Type := Self.Last;
            B : constant Count_Type := To_Count (Before);
         begin
            if L + Count > Self.Capacity then
               Storage.Resize (Self, L + Count, L, Force => False);
            end if;

            Storage.Copy
              (Self, Source => Self,
               Source_From  => B,
               Source_To    => L,
               Self_From    => B + Count);

            for J in B .. B + Count - 1 loop
               Storage.Set_Element
                 (Self, J, Storage.Elements.To_Stored (Element));
            end loop;

            Self.Last := Self.Last + Count;
         end;
      end if;
   end Insert;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Base_Vector'Class) is
      L : constant Count_Type := Self.Last;
   begin
      for J in Min_Index .. L loop
         Storage.Release_Element (Self, J);
      end loop;

      --  Deallocate all memory
      Storage.Resize (Self, 0, L, Force => True);
      Self.Last := No_Last;
   end Clear;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self  : in out Base_Vector'Class;
      Index : Index_Type;
      Count : Count_Type := 1)
   is
      Idx : constant Count_Type := To_Count (Index);
      Actual : constant Count_Type :=
        Count_Type'Min (Count, Self.Last - Idx + 1);
   begin
      for C in 0 .. Actual - 1 loop
         Storage.Release_Element (Self, Idx + C);
      end loop;

      Storage.Copy
        (Self, Source => Self,
         Source_From  => Idx + Actual,
         Source_To    => Self.Last,
         Self_From    => Idx);

      Self.Last := Self.Last - Actual;
   end Delete;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Self : in out Base_Vector'Class) is
   begin
      Storage.Release_Element (Self, Self.Last);
      Self.Last := Self.Last - 1;
   end Delete_Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element
     (Self : Base_Vector'Class) return Constant_Returned_Type is
   begin
      return Storage.Elements.To_Constant_Returned
        (Storage.Get_Element (Self, Self.Last));
   end Last_Element;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Self : in out Base_Vector'Class; Source : Base_Vector'Class) is
   begin
      Storage.Assign (Self, Source, Last => Source.Last);
      Self.Last := Source.Last;
   end Assign;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Base_Vector) is
   begin
      Assign (Self, Self);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Base_Vector) is
   begin
      Clear (Self);
   end Finalize;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Base_Vector'Class; Position : Index_Type)
         return Constant_Returned_Type
   is
   begin
      return Storage.Elements.To_Constant_Returned
        (Storage.Get_Element (Self, To_Count (Position)));
   end Element;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self : Base_Vector'Class; Position : Index_Type)
         return Returned_Type is
   begin
      return Storage.Elements.To_Returned
        (Storage.Get_Element (Self, To_Count (Position)));
   end Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Self     : in out Base_Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
   is
      Pos : constant Count_Type := To_Count (Index);
   begin
      Storage.Release_Element (Self, Pos);
      Storage.Set_Element
        (Self, Pos, Storage.Elements.To_Stored (New_Item));
   end Replace_Element;

   ----------
   -- Swap --
   ----------

   procedure Swap
     (Self        : in out Base_Vector'Class;
      Left, Right : Index_Type)
   is
      L     : constant Count_Type := To_Count (Left);
      R     : constant Count_Type := To_Count (Right);
      L_Tmp : Stored_Type := Storage.Get_Element (Self, L);
      R_Tmp : Stored_Type := Storage.Get_Element (Self, R);
   begin
      --  Since we will only keep one copy of the elements in the end, we
      --  should test Movable here, not Copyable.
      if Storage.Elements.Movable then
         Storage.Set_Element (Self, L, R_Tmp);
         Storage.Set_Element (Self, R, L_Tmp);

      else
         declare
            L2 : constant Stored_Type := Storage.Elements.Copy (L_Tmp);
            R2 : constant Stored_Type := Storage.Elements.Copy (R_Tmp);
         begin
            Storage.Release_Element (Self, L);
            Storage.Set_Element (Self, L, R2);
            Storage.Release_Element (Self, R);
            Storage.Set_Element (Self, R, L2);
         end;
      end if;
   end Swap;

end Conts.Vectors.Impl;
