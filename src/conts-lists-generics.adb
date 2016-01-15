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

package body Conts.Lists.Generics with SPARK_Mode => Off is

   use Nodes;

   ------------
   -- Append --
   ------------

   procedure Append
      (Self    : in out List'Class;
       Element : Element_Type)
   is
      N : Node_Access;
   begin
      Allocate
         (Self,
          Nodes.Elements.To_Stored (Element),
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

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out List'Class) is
      C : Cursor := Self.First;
      N : Cursor;
      E : Stored_Type;
   begin
      while Self.Has_Element (C) loop
         N := Self.Next (C);
         E := Get_Element (Self, C.Current);
         Elements.Release (E);
         Nodes.Release_Node (Self, C.Current);
         C := N;
      end loop;
      Nodes.Release (Self);

      Self.Head := Nodes.Null_Access;
      Self.Tail := Nodes.Null_Access;
      Self.Size := 0;
   end Clear;

   ------------
   -- Length --
   ------------

   function Length (Self : List'Class) return Count_Type is
   begin
      return Self.Size;
   end Length;

   -----------
   -- First --
   -----------

   function First (Self : List'Class) return Cursor is
   begin
      return (Current => Self.Head);
   end First;

   -------------
   -- Element --
   -------------

   function Element
      (Self : List'Class; Position : Cursor) return Return_Type is
   begin
      --  Precondition ensures there is an element at that position
      return Nodes.Elements.To_Return (Get_Element (Self, Position.Current));
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
      (Self : List'Class; Position : Cursor) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Position.Current /= Null_Access;
   end Has_Element;

   ----------
   -- Next --
   ----------

   function Next
      (Self : List'Class; Position : Cursor) return Cursor is
   begin
      if Position.Current = Null_Access then
         return Position;
      else
         return (Current => Get_Next (Self, Position.Current));
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous
      (Self : List'Class; Position : Cursor) return Cursor is
   begin
      if Position.Current = Null_Access then
         return Position;
      else
         return (Current => Get_Previous (Self, Position.Current));
      end if;
   end Previous;

   ----------
   -- Next --
   ----------

   procedure Next (Self : List'Class; Position : in out Cursor) is
   begin
      Position := Next (Self, Position);
   end Next;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out List) is
   begin
      Clear (Self);
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Self : in out List) is
   begin
      Nodes.Assign (Self, Self,
                    Self.Head, Self.Head,
                    Self.Tail, Self.Tail);
   end Adjust;

   ------------
   -- Assign --
   ------------

   procedure Assign (Self : in out List'Class; Source : List'Class) is
   begin
      if Self'Address = Source'Address then
         --  Tagged types are always passed by reference, so we know they
         --  are the same, and do nothing.
         return;
      end if;

      Nodes.Assign (Self, Source,
                    Self.Head, Source.Head,
                    Self.Tail, Source.Tail);
   end Assign;

end Conts.Lists.Generics;
