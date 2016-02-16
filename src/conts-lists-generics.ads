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
with Conts.Cursors;
with Conts.Lists.Storage;
with Conts.Properties;

generic
   with package Storage is new Conts.Lists.Storage.Traits (<>);
   --  Describes how the nodes of the list are stored.

package Conts.Lists.Generics with SPARK_Mode is

   subtype Element_Type is Storage.Elements.Element_Type;
   subtype Returned_Type is Storage.Elements.Returned_Type;
   subtype Stored_Type is Storage.Elements.Stored_Type;
   subtype Constant_Returned_Type is Storage.Elements.Constant_Returned_Type;

   package Impl is
      type Base_List is new Storage.Container with private;
      --  We do not define the Iterable aspect here: this is not allowed,
      --  since the parent type is a generic formal parameter. Instead, we
      --  have to define it in the instantiations of Generic_List.

      type Cursor is private;
      No_Element : constant Cursor;

      function Length (Self : Base_List'Class) return Count_Type
        with Inline => True, Global => null;
      function Capacity (Self : Base_List'Class) return Count_Type
        is (Storage.Capacity (Self))
        with Inline => True, Global => null;
      procedure Clear (Self : in out Base_List'Class);
      procedure Assign
        (Self : in out Base_List'Class; Source : Base_List'Class);
      procedure Append (Self : in out Base_List'Class; Element : Element_Type)
        with Global => null,
             Pre    => Length (Self) + 1 <= Capacity (Self);
      function First (Self : Base_List'Class) return Cursor
        with Inline, Global => null;
      function Has_Element
        (Self : Base_List'Class; Position : Cursor) return Boolean
        with Inline, Global => null;
      function Element
        (Self : Base_List'Class; Position : Cursor)
         return Constant_Returned_Type
        with Inline, Global => null,
             Pre    => Has_Element (Self, Position);
      function Next
        (Self : Base_List'Class; Position : Cursor) return Cursor
        with Inline, Global => null,
             Pre    => Has_Element (Self, Position);
      function Previous
        (Self : Base_List'Class; Position : Cursor) return Cursor
        with Inline, Global => null,
             Pre    => Has_Element (Self, Position);
      --  Actual implementation for the subprograms renamed below. See the
      --  descriptions below.

      function First_Primitive (Self : Base_List) return Cursor
         is (First (Self)) with Inline;
      function Element_Primitive
        (Self : Base_List; Position : Cursor) return Constant_Returned_Type
         is (Element (Self, Position)) with Inline;
      function Has_Element_Primitive
        (Self : Base_List; Position : Cursor) return Boolean
         is (Has_Element (Self, Position)) with Inline;
      function Next_Primitive
        (Self : Base_List; Position : Cursor) return Cursor
         is (Next (Self, Position)) with Inline;
      --  These are only needed because the Iterable aspect expects a parameter
      --  of type List instead of List'Class. But then it means that the loop
      --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
      --  using an explicit cursor.

   private
      pragma SPARK_Mode (Off);
      procedure Adjust (Self : in out Base_List);
      procedure Finalize (Self : in out Base_List);
      --  In case the list is a controlled type, but irrelevant when the list
      --  is not controlled.

      type Base_List is new Storage.Container with record
         Head, Tail : Storage.Node_Access := Storage.Null_Access;
         Size       : Natural := 0;
      end record;

      type Cursor is record
         Current : Storage.Node_Access;
      end record;
      No_Element : constant Cursor := (Current => Storage.Null_Access);
   end Impl;

   subtype Base_List is Impl.Base_List;
   subtype Cursor is Impl.Cursor;
   No_Element : constant Cursor := Impl.No_Element;

   procedure Append (Self : in out Base_List'Class; Element : Element_Type)
     renames Impl.Append;
   --  Append a new element to the list.
   --  Complexity: O(1)

   function Length (Self : Base_List'Class) return Count_Type
     renames Impl.Length;
   --  Return the number of elements in the list.
   --  Complexity: O(n)  (in practice, constant)

   function Capacity (Self : Base_List'Class) return Count_Type
     renames Impl.Capacity;
   --  Return the maximal number of elements in the list. This will be
   --  Count_Type'Last for unbounded containers.
   --  Complexity: constant

   procedure Clear (Self : in out Base_List'Class)
     renames Impl.Clear;
   --  Free the contents of the list
   --  Complexity:  O(n)

   procedure Assign (Self : in out Base_List'Class; Source : Base_List'Class)
     renames Impl.Assign;
   --  Replace all elements of Self with a copy of the elements of Source.
   --  When the list is controlled, this has the same behavior as calling
   --  Self := Source.
   --  Complexity: O(n)

   function First (Self : Base_List'Class) return Cursor
     renames Impl.First;
   function Element
     (Self : Base_List'Class; Position : Cursor) return Constant_Returned_Type
     renames Impl.Element;
   function Has_Element
     (Self : Base_List'Class; Position : Cursor) return Boolean
     renames Impl.Has_Element;
   function Next
     (Self : Base_List'Class; Position : Cursor) return Cursor
     renames Impl.Next;
   function Previous
     (Self : Base_List'Class; Position : Cursor) return Cursor
     renames Impl.Previous;
   --  We pass the container explicitly for the sake of writing the pre
   --  and post conditions.
   --  Complexity: constant for all cursor operations.

   procedure Next (Self : Base_List'Class; Position : in out Cursor)
      with Inline, Global => null,
           Pre    => Has_Element (Self, Position);

   --  ??? Should we provide a Copy function ?
   --  This cannot be provided in this generic package, since the type could
   --  be constrained and/or limited, so it has to be provided in all child
   --  packages. However, when the type is controlled it is much easier to
   --  just use the standard assignment operator, so providing Copy is not
   --  needed (only when base type is limited)

   ------------------
   -- for-of loops --
   ------------------

   type List is new Base_List with null record
     with Constant_Indexing => Constant_Reference,
          Iterable => (First       => First_Primitive,
                       Next        => Next_Primitive,
                       Has_Element => Has_Element_Primitive,
                       Element     => Element_Primitive);

   function Constant_Reference
     (Self : List; Position : Cursor) return Constant_Returned_Type
     is (Element (Self, Position)) with Inline;

   --------------------
   -- Cursors traits --
   --------------------

   package Cursors is
      package Bidirectional is new Conts.Cursors.Bidirectional_Cursors
        (Container_Type => Base_List'Class,
         Cursor_Type    => Cursor,
         First          => First,
         Next           => Next,
         Has_Element    => Has_Element,
         Previous       => Previous);
      package Forward renames Bidirectional.Forward;
   end Cursors;

   -------------------------
   -- Getters and setters --
   -------------------------

   function As_Element
     (Self : Base_List'Class; Position : Cursor) return Element_Type
     is (Storage.Elements.To_Element (Element (Self, Position)));

   package Maps is
      package Element is new Conts.Properties.Read_Only_Maps
        (Cursors.Forward.Container, Cursors.Forward.Cursor,
         Element_Type, As_Element);
      package Constant_Returned is new Conts.Properties.Read_Only_Maps
        (Cursors.Forward.Container, Cursors.Forward.Cursor,
         Storage.Elements.Constant_Returned, Conts.Lists.Generics.Element);
   end Maps;

end Conts.Lists.Generics;
