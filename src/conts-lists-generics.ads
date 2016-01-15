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

--  A generic and general list implementation
--  By providing appropriate values for the formal parameters, the same
--  implementation can be used for bounded and unbounded containers, or for
--  constrained and unconstrained elements.
--
--  Design: in C++ STL, none of the methods are virtual, so there is no
--  dynamic dispatching. We achieve the same here by using 'Class
--  parameters.  This still let's use Ada2012 dot notation (the reason why
--  we use a tagged type, in addition to the Iterable aspect), while
--  increasing the performance (the count-with-explicit-loop goes from 0.25s
--  to 0.51s when we do not use 'Class parameters).

pragma Ada_2012;
with Conts.Lists.Nodes;

generic
   with package Nodes is new Conts.Lists.Nodes.Traits (<>);
   --  Describes how the nodes of the list are stored.

package Conts.Lists.Generics with SPARK_Mode is

   type List is new Nodes.Container with private;
   --  We do not define the Iterable aspect here: this is not allowed,
   --  since the parent type is a generic formal parameter. Instead, we
   --  have to define it in the instantiations of Generic_List.

   subtype Element_Type is Nodes.Elements.Element_Type;
   subtype Return_Type is Nodes.Elements.Return_Type;
   subtype Stored_Type is Nodes.Elements.Stored_Type;

   type Cursor is private;
   No_Element : constant Cursor;

   procedure Append (Self : in out List'Class; Element : Element_Type)
      with Global => null,
           Pre    => Length (Self) <= Capacity (Self) - 1;
   --  Append a new element to the list.
   --  Complexity: O(1)

   function Length (Self : List'Class) return Count_Type
      with Inline => True,
           Global => null;
   --  Return the number of elements in the list.
   --  Complexity: O(n)  (in practice, constant)

   function Capacity (Self : List'Class) return Count_Type
      is (Nodes.Capacity (Self))
      with Inline => True,
           Global => null;
   --  Return the maximal number of elements in the list. This will be
   --  Count_Type'Last for unbounded containers.
   --  Complexity: constant

   procedure Clear (Self : in out List'Class);
   --  Free the contents of the list
   --  Complexity:  O(n)

   procedure Assign (Self : in out List'Class; Source : List'Class);
   --  Replace all elements of Self with a copy of the elements of Source.
   --  When the list is controlled, this has the same behavior as calling
   --  Self := Source.
   --  Complexity: O(n)

   function First (Self : List'Class) return Cursor
      with Inline,
           Global => null;
   function Element
      (Self : List'Class; Position : Cursor) return Return_Type
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);
   function Has_Element
      (Self : List'Class; Position : Cursor) return Boolean
      with Inline,
           Global => null;
   function Next
      (Self : List'Class; Position : Cursor) return Cursor
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);
   function Previous
      (Self : List'Class; Position : Cursor) return Cursor
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);
   --  We pass the container explicitly for the sake of writing the pre
   --  and post conditions.
   --  Complexity: constant for all cursor operations.

   procedure Next (Self : List'Class; Position : in out Cursor)
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);

   function First_Primitive (Self : List) return Cursor
      is (First (Self)) with Inline;
   function Element_Primitive
      (Self : List; Position : Cursor) return Return_Type
      is (Element (Self, Position)) with Inline;
   function Has_Element_Primitive
      (Self : List; Position : Cursor) return Boolean
      is (Has_Element (Self, Position)) with Inline;
   function Next_Primitive
      (Self : List; Position : Cursor) return Cursor
      is (Next (Self, Position)) with Inline;
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type List instead of List'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.

private
   pragma SPARK_Mode (Off);
   procedure Adjust (Self : in out List);
   procedure Finalize (Self : in out List);
   --  In case the list is a controlled type, but irrelevant when the list
   --  is not controlled.

   type List is new Nodes.Container with record
      Head, Tail : Nodes.Node_Access := Nodes.Null_Access;
      Size : Natural := 0;
   end record;

   type Cursor is record
      Current : Nodes.Node_Access;
   end record;

   No_Element : constant Cursor := (Current => Nodes.Null_Access);

end Conts.Lists.Generics;
