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

--  A vector abstract data type

pragma Ada_2012;
with Conts.Vectors.Nodes;

generic
   type Index_Type is range <>;
   with package Nodes is new Conts.Vectors.Nodes.Traits (<>);
package Conts.Vectors.Generics with SPARK_Mode is

   type Vector is new Nodes.Container with private;
   --  Define the iterable aspect later, since this is not allowed when the
   --  parent type is a generic formal.

   subtype Element_Type is Nodes.Elements.Element_Type;
   subtype Return_Type is Nodes.Elements.Return_Type;
   subtype Stored_Type is Nodes.Elements.Stored_Type;

   type Cursor is private;
   No_Element : constant Cursor;

   function To_Count (Idx : Index_Type) return Count_Type with Inline;
   --  Converts from an index type to an index into the actual underlying
   --  array.

   procedure Reserve_Capacity
     (Self : in out Vector'Class; Capacity : Count_Type);
   --  Make sure the vector is at least big enough to contain Capacity items
   --  (the vector must also be big enough to contain all its current
   --  elements)
   --  If you insert more items, the vector might be resized to a bigger
   --  capacity (when using unbounded nodes, for instance).
   --  If you remove items, a vector is never resized.
   --  If you clear the vector, it's capacity is reset to 0 and memory is
   --  freed if possible.

   procedure Shrink_To_Fit (Self : in out Vector'Class);
   --  Resize the vector to fit its number of elements. This might free
   --  memory.

   function Length (Self : Vector'Class) return Count_Type
     with Inline => True, Global => null;
   --  Return the number of elements in Self.

   function Is_Empty (Self : Vector'Class) return Boolean
      is (Self.Length = 0) with Inline;
   --  Whether the vector is empty

   function Element
     (Self : Vector'Class; Position : Index_Type) return Return_Type;

   procedure Replace_Element
     (Self     : in out Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
     with Global => null,
          Pre    => Count_Type (Index) - Conts.Vectors.Nodes.Min_Index + 1
                    <= Length (Self);
   --  Replace the element at the given position.
   --  Nothing is done if Index is not a valid index in the container.

   procedure Append
     (Self    : in out Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
     with Global => null,
          Pre    => Length (Self) + Count <= Nodes.Max_Capacity (Self);
   --  Append Count copies of Element to the vector, increasing the capacity
   --  as needed.

   procedure Clear (Self : in out Vector'Class);
   --  Remove all contents from the vector.

   procedure Delete (Self : in out Vector'Class; Index : Index_Type)
     with Pre => Self.Length >= To_Count (Index);
   --  Remove an element from the vector.
   --  Unless you are removing the last element (see Delete_Last), this is an
   --  inefficient operation since it needs to copy all the elements after
   --  the one being removed.

   procedure Delete_Last (Self : in out Vector'Class)
     with Global => null, Pre => not Self.Is_Empty;
   --  Remove the last element from the vector.
   --  The vector is not resized, so it will keep its current capacity, for
   --  efficient insertion of future elements. You can call Shrink_To_Fit

   function Last_Element (Self : Vector'Class) return Return_Type
     with Global => null, Pre => not Self.Is_Empty;
   --  Return the last element in the vector.

   procedure Assign (Self : in out Vector'Class; Source : Vector'Class);
   --  Replace all elements of Self with a copy of the elements of Source.
   --  When the list is controlled, this has the same behavior as calling
   --  Self := Source.

   function First (Self : Vector'Class) return Cursor
      with Inline,
           Global => null;
   function Element
      (Self : Vector'Class; Position : Cursor) return Return_Type
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);
   function Has_Element
      (Self : Vector'Class; Position : Cursor) return Boolean
      with Inline,
           Global => null;
   function Next
      (Self : Vector'Class; Position : Cursor) return Cursor
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);
   function Previous
      (Self : Vector'Class; Position : Cursor) return Cursor
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);
   --  We pass the container explicitly for the sake of writing the pre
   --  and post conditions.
   --  Complexity: constant for all cursor operations.

   procedure Next (Self : Vector'Class; Position : in out Cursor)
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position);

   function First_Primitive (Self : Vector) return Cursor
      is (First (Self)) with Inline;
   function Element_Primitive
      (Self : Vector; Position : Cursor) return Return_Type
      is (Element (Self, Position)) with Inline;
   function Has_Element_Primitive
      (Self : Vector; Position : Cursor) return Boolean
      is (Has_Element (Self, Position)) with Inline;
   function Next_Primitive
      (Self : Vector; Position : Cursor) return Cursor
      is (Next (Self, Position)) with Inline;
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type List instead of List'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.

private
   pragma SPARK_Mode (Off);
   procedure Adjust (Self : in out Vector);
   procedure Finalize (Self : in out Vector);
   --  In case the list is a controlled type, but irrelevant when Self
   --  is not controlled.

   function To_Count (Idx : Index_Type) return Count_Type
   is (Count_Type
       (Conts.Vectors.Nodes.Min_Index
        + Count_Type'Base (Idx)
        - Count_Type'Base (Index_Type'First)));

   type Cursor is record
      Index   : Count_Type;
   end record;

   No_Element : constant Cursor :=
     (Index => Conts.Vectors.Nodes.Min_Index - 1);

   type Vector is new Nodes.Container with record
      Last  : Count_Type := No_Element.Index;
      --  Last assigned element
   end record;

end Conts.Vectors.Generics;
