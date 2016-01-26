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

package body Conts.Vectors.Generics is
   use Conts.Vectors.Nodes;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Self : in out Vector'Class; Capacity : Count_Type) is
   begin
      Nodes.Resize
         (Self, Count_Type'Max (Self.Last, Capacity),
          Self.Last, Force => True);
   end Reserve_Capacity;

   -------------------
   -- Shrink_To_Fit --
   -------------------

   procedure Shrink_To_Fit (Self : in out Vector'Class) is
   begin
      Nodes.Resize (Self, Self.Last, Self.Last, Force => True);
   end Shrink_To_Fit;

   ------------
   -- Length --
   ------------

   function Length (Self : Vector'Class) return Count_Type is
   begin
      return Self.Last - Min_Index + 1;
   end Length;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Vector'Class; Position : Index_Type) return Return_Type is
   begin
      return Nodes.Elements.To_Return
        (Nodes.Get_Element (Self, To_Count (Position)));
   end Element;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Self     : in out Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
   is
      Pos : constant Count_Type := To_Count (Index);
   begin
      --  ??? This extra check is only necessary for safety, can we move it to
      --  a Check_Policy package ?
      if Pos < Self.Last then
         Nodes.Release_Element (Self, Pos);
         Nodes.Set_Element (Self, Pos, Nodes.Elements.To_Stored (New_Item));
      end if;
   end Replace_Element;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self    : in out Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
   is
      L : constant Count_Type := Self.Last;
   begin
      if L + Count > Self.Capacity then
         Nodes.Resize (Self, L + Count, L, Force => False);
      end if;
      for J in 1 .. Count loop
         Nodes.Set_Element
           (Self, L + J, Nodes.Elements.To_Stored (Element));
      end loop;
      Self.Last := Self.Last + Count;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Vector'Class) is
      L : constant Count_Type := Self.Last;
   begin
      for J in Min_Index .. L loop
         Nodes.Release_Element (Self, J);
      end loop;

      --  Deallocate all memory
      Nodes.Resize (Self, 0, L, Force => True);
      Self.Last := Min_Index - 1;
   end Clear;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out Vector'Class; Index : Index_Type) is
      Idx : constant Count_Type := To_Count (Index);
   begin
      Nodes.Release_Element (Self, Idx);
      Nodes.Copy
        (Self, Source => Self,
         Source_From  => Idx + 1,
         Source_To    => Self.Last,
         Self_From    => Idx);
      Self.Last := Self.Last - 1;
   end Delete;

   -----------------
   -- Delete_Last --
   -----------------

   procedure Delete_Last (Self : in out Vector'Class) is
   begin
      Nodes.Release_Element (Self, Self.Last);
      Self.Last := Self.Last - 1;
   end Delete_Last;

   ------------------
   -- Last_Element --
   ------------------

   function Last_Element (Self : Vector'Class) return Return_Type is
   begin
      return Nodes.Elements.To_Return (Nodes.Get_Element (Self, Self.Last));
   end Last_Element;

   ------------
   -- Assign --
   ------------

   procedure Assign (Self : in out Vector'Class; Source : Vector'Class) is
   begin
      Nodes.Assign (Self, Source, Last => Source.Last);
      Self.Last := Source.Last;
   end Assign;

   -----------
   -- First --
   -----------

   function First (Self : Vector'Class) return Cursor is
      pragma Unreferenced (Self);
   begin
      return (Index => Min_Index);
   end First;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Vector'Class; Position : Cursor) return Return_Type is
   begin
      return Nodes.Elements.To_Return
        (Nodes.Get_Element (Self, Position.Index));
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Vector'Class; Position : Cursor) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Position /= No_Element;
   end Has_Element;

   ----------
   -- Next --
   ----------

   function Next
     (Self : Vector'Class; Position : Cursor) return Cursor is
   begin
      if Position.Index < Self.Last then
         return (Index => Position.Index + 1);
      else
         return No_Element;
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Self : Vector'Class; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Self);
   begin
      if Position.Index > Min_Index then
         return (Index => Position.Index - 1);
      else
         return No_Element;
      end if;
   end Previous;

   ----------
   -- Next --
   ----------

   procedure Next (Self : Vector'Class; Position : in out Cursor) is
   begin
      if Position.Index < Self.Last then
         Position.Index := Position.Index + 1;
      else
         Position := No_Element;
      end if;
   end Next;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Vector) is
   begin
      Assign (Self, Self);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Vector) is
   begin
      Clear (Self);
   end Finalize;

end Conts.Vectors.Generics;
