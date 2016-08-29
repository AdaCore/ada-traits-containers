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
with Ada.Unchecked_Conversion;
with System;                 use System;

package body Conts.Lists.Impl with SPARK_Mode => Off is

   use Storage;

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   ---------------
   -- Positions --
   ---------------

   function Positions (Self : Base_List'Class) return P_Map is
      Position : Cursor := (Current => Self.Head);
      R        : P.Map;
      I        : Count_Type := 1;
   begin
      while Position.Current /= Null_Access loop
         R := P.Add (R, Position, I);
         Position := (Current => Get_Next (Self, Position.Current));
         I := I + 1;
      end loop;
      return P_Map'(Content => R);
   end Positions;

   -----------
   -- Model --
   -----------

   function Model (Self : Base_List'Class) return M.Sequence is
      Position : Cursor := (Current => Self.Head);
      R        : M.Sequence;
   begin
      while Position.Current /= Null_Access loop
         R := M.Add (R, Storage.Elements.To_Element
                     (Storage.Elements.To_Constant_Returned
                      (Get_Element (Self, Position.Current))));
         Position := (Current => Get_Next (Self, Position.Current));
      end loop;
      return R;
   end Model;

   ----------------------------
   -- Lift_Abstraction_Level --
   ----------------------------

   procedure Lift_Abstraction_Level (Self : Base_List'Class) is null;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Base_List'Class) is
      C : Cursor := Self.First;
      N : Cursor;
      E : Stored_Type;
   begin
      while C.Current /= Null_Access loop
         N := (Current => Get_Next (Self, C.Current));
         E := Get_Element (Self, C.Current);
         Elements.Release (E);
         Storage.Release_Node (Self, C.Current);
         C := N;
      end loop;
      Storage.Release (Self);

      Self.Head := Storage.Null_Access;
      Self.Tail := Storage.Null_Access;
      Self.Size := 0;
   end Clear;

   -----------
   -- First --
   -----------

   function First (Self : Base_List'Class) return Cursor is
   begin
      return (Current => Self.Head);
   end First;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Base_List'Class; Position : Cursor)
         return Constant_Returned_Type is
   begin
      return Storage.Elements.To_Constant_Returned
        (Get_Element (Self, Position.Current));
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Base_List'Class; Position : Cursor) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Position.Current /= Null_Access;
   end Has_Element;

   -----------
   -- Last --
   -----------

   function Last (Self : Base_List'Class) return Cursor is
   begin
      return (Current => Self.Tail);
   end Last;

   ----------
   -- Next --
   ----------

   function Next
     (Self : Base_List'Class; Position : Cursor) return Cursor is
   begin
      if Position.Current = Null_Access then
         return Position;
      else
         return (Current => Get_Next (Self, Position.Current));
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Self : Base_List'Class; Position : in out Cursor) is
   begin
      Position := Next (Self, Position);
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
     (Self : Base_List'Class; Position : Cursor) return Cursor is
   begin
      if Position.Current = Null_Access then
         return Position;
      else
         return (Current => Get_Previous (Self, Position.Current));
      end if;
   end Previous;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self    : in out Base_List'Class;
      Element : Element_Type)
   is
      N : Node_Access;
   begin
      Allocate
        (Self,
         Storage.Elements.To_Stored (Element),
         New_Node => N);

      if Self.Tail = Null_Access then
         Self.Tail := N;
         Self.Head := Self.Tail;
      else
         Set_Next (Self, Self.Tail, Next => N);
         Set_Previous (Self, N, Previous => Self.Tail);
         Self.Tail := N;
      end if;

      Self.Size := Self.Size + 1;
   end Append;

   ------------
   -- Length --
   ------------

   function Length (Self : Base_List'Class) return Count_Type is
   begin
      return Self.Size;
   end Length;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out Base_List) is
   begin
      Storage.Assign (Self, Self,
                      Self.Head, Self.Head,
                      Self.Tail, Self.Tail);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Base_List) is
   begin
      Clear (Self);
   end Finalize;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Self : in out Base_List'Class; Source : Base_List'Class) is
   begin
      if Self'Address = Source'Address then
         --  Tagged types are always passed by reference, so we know they
         --  are the same, and do nothing.
         return;
      end if;

      Storage.Assign (Self, Source,
                      Self.Head, Source.Head,
                      Self.Tail, Source.Tail);
      Self.Size := Source.Size;
   end Assign;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self : in out Base_List'Class; Position : Cursor; Element : Element_Type)
   is
   begin
      raise Program_Error with "Not implemented yet";
   end Insert;

   -------------------------
   -- Replacement_Element --
   -------------------------

   procedure Replace_Element
     (Self : in out Base_List'Class; Position : Cursor; Element : Element_Type)
   is
   begin
      raise Program_Error with "Not implemented yet";
   end Replace_Element;

end Conts.Lists.Impl;
