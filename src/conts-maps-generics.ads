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
with Conts.Cursors;
with Conts.Elements;
with Conts.Properties;
with Conts.Maps.Impl;

generic
   with package Keys is new Conts.Elements.Traits (<>);
   with package Elements is new Conts.Elements.Traits (<>);
   type Container_Base_Type is abstract tagged limited private;

   with function Hash (Key : Keys.Element_Type) return Hash_Type;

   type Probing is new Probing_Strategy with private;

   with package Pool is new Conts.Pools (<>);
   --  The storage pool used to allocate the buckets

   with function "="
     (Left  : Keys.Element_Type;
      Right : Keys.Stored_Type) return Boolean is <>;
   --  Compares a key given by the user with a stored key. For efficiency
   --  reasons, we do not convert Right back to Keys.Element_Type, although
   --  of course your function could do it with Keys.To_Return and
   --  Keys.To_Element.

   with function Resize_Strategy
     (Used     : Count_Type;
      Fill     : Count_Type;
      Capacity : Count_Type) return Count_Type is Resize_2_3;
   --  This function decides whether the hash-table needs to be resized.
   --  Resizing is a costly operation, so should be performed as rarely
   --  as possible. On the other hand, a table should have plenty of empty
   --  slots to make inserting efficient.
   --  If this function returns 0, no resizing takes place and the current
   --  capacity is preserved. Otherwise, it returns the new desired size
   --
   --  Used is the number of slots used to store elements.
   --  Fill is the number of slots used to store elements or previously
   --  used for now-deleted elements.
   --  Capacity is the maximum number of elements that can be stored.

package Conts.Maps.Generics with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Key_Type is Keys.Element_Type;
   subtype Element_Type is Elements.Element_Type;
   subtype Returned_Type is Elements.Returned_Type;
   subtype Constant_Returned_Type is Elements.Constant_Returned_Type;
   subtype Constant_Returned_Key_Type is Keys.Constant_Returned_Type;

   package Impl is new Conts.Maps.Impl
     (Keys                => Keys,
      Elements            => Elements,
      Container_Base_Type => Container_Base_Type,
      Hash                => Hash,
      Probing             => Probing,
      Pool                => Pool,
      "="                 => "=",
      Resize_Strategy     => Resize_Strategy);

   subtype Base_Map is Conts.Maps.Generics.Impl.Base_Map;
   subtype Cursor is Impl.Cursor;
   use all type Impl.Cursor;
   No_Element : constant Cursor := Impl.No_Element;

   function Capacity (Self : Base_Map'Class) return Count_Type
     renames Impl.Capacity;
   --  Return the current capacity of the container.

   function Length (Self : Base_Map'Class) return Count_Type
     renames Impl.Length;
   --  Return the number of elements contained in the container.

   procedure Resize
     (Self     : in out Base_Map'Class;
      New_Size : Count_Type) renames Impl.Resize;
   --  Change the capacity of the container.
   --  If you know you are going to insert n items, calling Resize (n) is
   --  likely to improve the performance by limiting the number of times the
   --  map will be resized during the insertions.
   --  Resize will always keep a capacity greater than the number of elements
   --  currently in the map.

   procedure Set
     (Self     : in out Base_Map'Class;
      Key      : Keys.Element_Type;
      Value    : Elements.Element_Type) renames Impl.Set;
   --  This never resizes Self if there is already an element with that key
   --  in the table. That means it is safe to iterate over a map and change
   --  some of the elements, but not insert new ones or remove ones.

   function Get
     (Self     : Base_Map'Class;
      Key      : Keys.Element_Type)
      return Elements.Constant_Returned_Type
     renames Impl.Get;
   --  Raises a Constraint_Error if there is no such element in the table

   procedure Delete
     (Self     : in out Base_Map'Class;
      Key      : Keys.Element_Type)
     renames Impl.Delete;
   --  Remove the element from the map.
   --  No exception is raised if the element is not in the map.

   procedure Clear (Self : in out Base_Map'Class) renames Impl.Clear;
   --  Remove all elements from the map

   function Key
     (Self : Base_Map'Class; Position : Cursor)
     return Constant_Returned_Key_Type
     renames Impl.Key;
   function Element
     (Self : Base_Map'Class; Position : Cursor) return Constant_Returned_Type
     renames Impl.Element;
   function As_Key
     (Self : Base_Map'Class; Position : Cursor) return Keys.Element_Type
     renames Impl.As_Key;
   function As_Element
     (Self : Base_Map'Class; Position : Cursor) return Elements.Element_Type
     renames Impl.As_Element;
   function As_Element
     (Self : Base_Map'Class; Key : Keys.Element_Type)
     return Elements.Element_Type
     renames Impl.As_Element;

   function First (Self : Base_Map'Class) return Cursor
     renames Impl.First;
   function Has_Element
     (Self : Base_Map'Class; Position : Cursor) return Boolean
     renames Impl.Has_Element;
   function Next
     (Self : Base_Map'Class; Position : Cursor) return Cursor
     renames Impl.Next;

   function Element
     (S : Impl.M.Map; K : Key_Type) return Element_Type
     renames Impl.Element;
   function S_Keys (Self : Base_Map'Class) return Impl.K.Sequence
     renames Impl.S_Keys;

   ------------------
   -- for-of loops --
   ------------------

   type Map is new Base_Map with null record
     with Constant_Indexing => Constant_Reference,
          Iterable => (First       => First_Primitive,
                       Next        => Next_Primitive,
                       Has_Element => Has_Element_Primitive,
                       Element     => Key_Primitive);
   --  Iteration can be done with either:
   --      for K of Self loop        --  get the keys
   --          Self.Get (K)          --  extra lookup to get the element
   --  or
   --      for C of Self loop        --  get a cursor
   --          Self.Key (C)          --  get the key (fast)
   --          Self.Element (C);     --  get the element (fast)

   function Constant_Reference
     (Self : Map; Key : Key_Type) return Constant_Returned_Type
     is (Get (Self, Key))
     with
       Inline,
       Pre'Class => Impl.M.Mem (Model (Self), Key);

   function Model (Self : Map'Class) return Impl.M.Map
     is (Impl.Model (Self))
     with Ghost;
   pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

   -------------
   -- Cursors --
   -------------

   package Cursors is
      package Forward is new Conts.Cursors.Forward_Cursors
        (Container_Type => Base_Map'Class,
         Cursor_Type    => Cursor,
         No_Element     => No_Element,
         First          => First,
         Next           => Next,
         Has_Element    => Has_Element);
   end Cursors;

   -------------------------
   -- Getters and setters --
   -------------------------

   package Maps is
      package Key is new Conts.Properties.Read_Only_Maps
        (Base_Map'Class, Cursor, Key_Type, As_Key);
      package Element is new Conts.Properties.Read_Only_Maps
        (Base_Map'Class, Cursor, Element_Type, As_Element);
      package Constant_Returned is new Conts.Properties.Read_Only_Maps
        (Base_Map'Class, Cursor, Elements.Constant_Returned,
         Conts.Maps.Generics.Element);
      package Constant_Returned_Key is new Conts.Properties.Read_Only_Maps
        (Base_Map'Class, Cursor, Keys.Constant_Returned,
         Conts.Maps.Generics.Key);
   end Maps;

end Conts.Maps.Generics;
