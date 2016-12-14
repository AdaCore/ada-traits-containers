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
with Conts.Lists.Impl;
with Conts.Properties;

generic
   with package Storage is new Conts.Lists.Storage.Traits (<>);
   --  Describes how the nodes of the list are stored.

package Conts.Lists.Generics with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Element_Type is Storage.Elements.Element_Type;
   subtype Returned_Type is Storage.Elements.Returned_Type;
   subtype Stored_Type is Storage.Elements.Stored_Type;
   subtype Constant_Returned_Type is Storage.Elements.Constant_Returned_Type;

   package Impl is new Conts.Lists.Impl (Storage);

   subtype Base_List is Impl.Base_List;
   subtype Cursor is Impl.Cursor;
   No_Element : constant Cursor := Impl.No_Element;
   use type Cursor;

   procedure Append
     (Self    : in out Base_List'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
     renames Impl.Append;
   --  Append Count copies of Element to the list.
   --  Complexity: O(1)

   procedure Insert
     (Self    : in out Base_List'Class;
      Before  : Cursor;
      Element : Element_Type;
      Count   : Count_Type := 1)
      renames Impl.Insert;
   --  Insert Count copies of Element before the element at position Before.
   --  If Before is No_Element, the copies are appended to the list.
   --  Complexity: O(1)

   function Length (Self : Base_List'Class) return Count_Type
     renames Impl.Length;
   --  Return the number of elements in the list.
   --  Complexity: O(n)  (in practice, constant)

   function Is_Empty (Self : Base_List'Class) return Boolean
      renames Impl.Is_Empty;
   --  Whether the list is empty.
   --  Complexity: O(1)

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
     renames Impl.Next;

   function "=" (Left, Right : Cursor) return Boolean renames Impl."=";

   use type Element_Type;

   function As_Element
     (Self : Base_List'Class; Position : Cursor) return Element_Type
     is (Storage.Elements.To_Element (Element (Self, Position)))
     with
       Pre  => Impl.P_Mem (Impl.Positions (Self), Position),
       Post => As_Element'Result = Element
          (Impl.Model (Self), Impl.P_Get (Impl.Positions (Self), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, As_Element);

   procedure Replace_Element
     (Self     : in out Base_List'Class;
      Position : Cursor;
      Element  : Element_Type)
      renames Impl.Replace_Element;
   --  If Position is a valid position in the container, it replaces the
   --  element at that position with Element.

   procedure Delete
     (Self     : in out Base_List'Class;
      Position : in out Cursor;
      Count    : Count_Type := 1)
     renames Impl.Delete;
   --  Delete Count elements starting at Position, or all the elements starting
   --  at Position if there are fewer than Count elements starting at
   --  Position).
   --  Finally, sets Position to the first item after the deleted ones.

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
     is (Element (Self, Position))
     with
       Inline,
       Pre'Class => Impl.P_Mem (Impl.Positions (Self), Position);

   -----------
   -- Model --
   -----------
   --  The following subprograms are used to write loop invariants for SPARK

   function Model (Self : List'Class) return Impl.M.Sequence
     is (Impl.Model (Self))
     with Ghost;
   pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

   function Element (S : Impl.M.Sequence; I : Count_Type) return Element_Type
     renames Impl.Element;

   --------------------
   -- Cursors traits --
   --------------------

   package Cursors is
      package Bidirectional is new Conts.Cursors.Bidirectional_Cursors
        (Container_Type => Base_List'Class,
         Cursor_Type    => Cursor,
         No_Element     => No_Element,
         First          => First,
         Next           => Next,
         Has_Element    => Has_Element,
         Previous       => Previous);
      package Forward renames Bidirectional.Forward;
   end Cursors;

   -------------------------
   -- Getters and setters --
   -------------------------

   package Maps is
      package Element is new Conts.Properties.Read_Only_Maps
        (Cursors.Forward.Container, Cursors.Forward.Cursor,
         Element_Type, As_Element);
      package Constant_Returned is new Conts.Properties.Read_Only_Maps
        (Cursors.Forward.Container, Cursors.Forward.Cursor,
         Storage.Elements.Constant_Returned, Conts.Lists.Generics.Element);
   end Maps;

end Conts.Lists.Generics;
