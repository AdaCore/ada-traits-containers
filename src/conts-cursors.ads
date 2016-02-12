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

--  This package provides signature packages that describe how to iterate over
--  containers, or to point to containers objects.

pragma Ada_2012;

package Conts.Cursors with SPARK_Mode is

   -----------------------------
   -- Constant_Forward_Traits --
   -----------------------------
   --  A package that describes how to use forward cursors.  Each container
   --  for which this is applicable provides an instance of this package,
   --  and algorithms should take this package as a generic parameter.

   generic
      type Container_Type (<>) is limited private;
      type Cursor_Type is private;
      type Returned_Type (<>) is private;
      with function First (Self : Container_Type) return Cursor_Type is <>;
      with function Element (Self : Container_Type; Pos : Cursor_Type)
         return Returned_Type is <>;
      with function Has_Element (Self : Container_Type; Pos : Cursor_Type)
         return Boolean is <>;
      with function Next (Self : Container_Type; Pos : Cursor_Type)
         return Cursor_Type is <>;

   package Constant_Forward_Traits is
      subtype Container is Container_Type;
      subtype Cursor    is Cursor_Type;
      subtype Returned  is Returned_Type;
   end Constant_Forward_Traits;

   -----------------------------------
   -- Constant_Bidirectional_Traits --
   -----------------------------------

   generic
      type Container_Type (<>) is limited private;
      type Cursor_Type is private;
      type Returned_Type (<>) is private;
      with function First (Self : Container_Type) return Cursor_Type is <>;
      with function Element (Self : Container_Type; Pos : Cursor_Type)
         return Returned_Type is <>;
      with function Has_Element (Self : Container_Type; Pos : Cursor_Type)
         return Boolean is <>;
      with function Next (Self : Container_Type; Pos : Cursor_Type)
         return Cursor_Type is <>;
      with function Previous (Self : Container_Type; Pos : Cursor_Type)
         return Cursor_Type is <>;

   package Constant_Bidirectional_Traits is
      subtype Container is Container_Type;
      subtype Cursor    is Cursor_Type;
      subtype Returned  is Returned_Type;

      --  A bidirectional cursor is also a forward cursor
      package Constant_Forward is new Constant_Forward_Traits
         (Container, Cursor, Returned_Type);
   end Constant_Bidirectional_Traits;

   -----------------------------
   -- Cursors on Element_Type --
   -----------------------------
   --  The above cursor packages will return a Returned_Type, not an
   --  Element_Type as stored in the container.
   --  For some containers, a Returned_Type is in fact a reference type, as a
   --  way to efficiently return unconstrained types without copying them.
   --  However, algorithms often expect to work on the original Element_Type,
   --  so the following provides a wrapper with a conversion function.
   --
   --  Design: we did not put these conversion functions in the Element_Traits
   --  packages because a cursor traits does not have access to these element
   --  traits.

   generic
      with package Cursors is new Constant_Forward_Traits (<>);
      type Element_Type (<>) is private;
      with function Convert
         (E : Cursors.Returned_Type) return Element_Type is <>;
   package Constant_Forward_Convert_Traits is
      subtype Element is Element_Type;
   end Constant_Forward_Convert_Traits;

   -----------------------------
   -- Constant Random cursors --
   -----------------------------
   --  These are cursors that can access any element from a container, not in
   --  any specific order.

   generic
      type Container_Type (<>) is limited private;
      type Index_Type is (<>);
      type Returned_Type (<>) is private;

      with function First (Self : Container_Type) return Index_Type is <>;
      --  Index of the first element in the container (often Index_Type'First)
      --  ??? Can we remove this parameter and always use Index_Type'First

      with function Element (Self : Container_Type; Pos : Index_Type)
        return Returned_Type is <>;
      --  Access any element of the container

      with function Last (Self : Container_Type) return Index_Type is <>;
      --  Return the index of the last valid element in the container.
      --  We do not use a Has_Element function, since having an explicit range
      --  is more convenient for algorithms (for instance to select random
      --  elements in the container).

      with function "-" (Left, Right : Index_Type) return Integer is <>;
      --  Return the number of elements between the two positions.

      with function "+"
        (left : Index_Type; N : Integer) return Index_Type is <>;
      --  Move Left forward or backward by a number of position.

   package Constant_Random_Traits is
      subtype Container is Container_Type;
      subtype Index     is Index_Type;
      subtype Returned  is Returned_Type;

      function "-" (Left : Index_Type; N : Integer) return Index_Type
        is (Left + (-N)) with Inline;

      function Next
        (Self : Container_Type; Idx : Index_Type)
         return Index_Type
        is (Idx + 1) with Inline;

      function Previous
        (Self : Container_Type; Idx : Index_Type)
         return Index_Type
        is (Idx - 1) with Inline;

      function Has_Element
        (Self : Container_Type; Idx : Index_Type) return Boolean
        is (Idx >= First (Self) and then Idx <= Last (Self)) with Inline;
      --  This might be made efficient if you pass a First function that
      --  returns a constant and if this contstant is Index_Type'First then
      --  the compiler can simply remove the test.

      --  A random cursor is also a bidirectional and forward cursor
      package Constant_Bidirectional is new Constant_Bidirectional_Traits
        (Container, Index_Type, Returned_Type);
      package Constant_Forward renames Constant_Bidirectional.Constant_Forward;
   end Constant_Random_Traits;

   --------------------
   -- Random cursors --
   --------------------

   generic
      type Container_Type (<>) is limited private;
      type Index_Type is (<>);
      type Returned_Type (<>) is private;
      type Element_Type (<>) is private;

      with function First (Self : Container_Type) return Index_Type is <>;
      --  Index of the first element in the container (often Index_Type'First)
      --  ??? Can we remove this parameter and always use Index_Type'First

      with function Element (Self : Container_Type; Pos : Index_Type)
                             return Returned_Type is <>;
      --  Access any element of the container

      with function Last (Self : Container_Type) return Index_Type is <>;
      --  Return the index of the last valid element in the container.
      --  We do not use a Has_Element function, since having an explicit range
      --  is more convenient for algorithms (for instance to select random
      --  elements in the container).

      with function "-" (Left, Right : Index_Type) return Integer is <>;
      --  Return the number of elements between the two positions.

      with function "+"
        (left : Index_Type; N : Integer) return Index_Type is <>;
      --  Move Left forward or backward by a number of position.

      with procedure Set_Element
        (Self     : in out Container_Type;
         Position : Index_Type;
         Value    : Element_Type) is <>;
      --  Modify the element at the given position

   package Random_Traits is
      subtype Container is Container_Type;
      subtype Index     is Index_Type;
      subtype Returned  is Returned_Type;

      package Constant_Random is new Constant_Random_Traits
        (Container_Type, Index_Type, Returned_Type);
      package Constant_Bidirectional
         renames Constant_Random.Constant_Bidirectional;
      package Constant_Forward renames Constant_Bidirectional.Constant_Forward;

      function Has_Element
        (Self : Container; Idx : Index) return Boolean
         renames Constant_Random.Has_Element;
      function "-" (Left : Index; N : Integer) return Index
        renames Constant_Random."-";

   end Random_Traits;

end Conts.Cursors;
