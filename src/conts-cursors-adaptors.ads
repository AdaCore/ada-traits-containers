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

--  This file provides adaptors for the standard Ada2012 containers, so that
--  they can be used with the algorithms declared in our containers hierarchy

pragma Ada_2012;
with Ada.Containers.Bounded_Doubly_Linked_Lists;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;

package Conts.Cursors.Adaptors is

   ---------------------------------------------
   -- Adaptor for bounded doubly linked lists --
   ---------------------------------------------

   generic
      with package Lists is
          new Ada.Containers.Bounded_Doubly_Linked_Lists (<>);
   package Bounded_List_Adaptors is
      subtype Element_Type is Lists.Element_Type;
      subtype List is Lists.List;
      subtype Cursor is Lists.Cursor;

      function First (Self : List) return Cursor
         renames Lists.First;
      function Element (Self : List; Position : Cursor) return Element_Type
         is (Lists.Element (Position)) with Inline;
      function Has_Element (Self : List; Position : Cursor) return Boolean
         is (Lists.Has_Element (Position)) with Inline;
      function Next (Self : List; Position : Cursor) return Cursor
         is (Lists.Next (Position)) with Inline;
      function Previous (Self : List; Position : Cursor) return Cursor
         is (Lists.Previous (Position)) with Inline;

      package Cursors is
         package Constant_Bidirectional is new Constant_Bidirectional_Traits
            (Container    => List'Class,
             Cursor       => Cursor,
             Return_Type  => Element_Type);
         package Constant_Forward
            renames Constant_Bidirectional.Constant_Forward;
      end Cursors;
   end Bounded_List_Adaptors;

   -------------------------------------
   -- Adaptor for doubly linked lists --
   -------------------------------------

   generic
      with package Lists is new Ada.Containers.Doubly_Linked_Lists (<>);
   package List_Adaptors is
      subtype Element_Type is Lists.Element_Type;
      subtype List is Lists.List;
      subtype Cursor is Lists.Cursor;

      function First (Self : List) return Cursor
         renames Lists.First;
      function Element (Self : List; Position : Cursor) return Element_Type
         is (Lists.Element (Position)) with Inline;
      function Has_Element (Self : List; Position : Cursor) return Boolean
         is (Lists.Has_Element (Position)) with Inline;
      function Next (Self : List; Position : Cursor) return Cursor
         is (Lists.Next (Position)) with Inline;
      function Previous (Self : List; Position : Cursor) return Cursor
         is (Lists.Previous (Position)) with Inline;

      package Cursors is
         package Constant_Bidirectional is new Constant_Bidirectional_Traits
            (Container    => List'Class,
             Cursor       => Cursor,
             Return_Type  => Element_Type);
         package Constant_Forward
            renames Constant_Bidirectional.Constant_Forward;
      end Cursors;
   end List_Adaptors;

   ------------------------------------------------
   -- Adaptor for indefinite doubly linked lists --
   ------------------------------------------------

   generic
      with package Lists is
         new Ada.Containers.Indefinite_Doubly_Linked_Lists (<>);
   package Indefinite_List_Adaptors is
      subtype Element_Type is Lists.Element_Type;
      subtype Return_Type is Lists.Constant_Reference_Type;
      subtype List is Lists.List;
      subtype Cursor is Lists.Cursor;

      function First (Self : List) return Cursor
         renames Lists.First;
      function Element (Self : List; Position : Cursor) return Return_Type
         is (Lists.Constant_Reference (Self, Position)) with Inline;
      function Has_Element (Self : List; Position : Cursor) return Boolean
         is (Lists.Has_Element (Position)) with Inline;
      function Next (Self : List; Position : Cursor) return Cursor
         is (Lists.Next (Position)) with Inline;
      function Previous (Self : List; Position : Cursor) return Cursor
         is (Lists.Previous (Position)) with Inline;

      package Cursors is
         package Constant_Bidirectional is new Constant_Bidirectional_Traits
            (Container    => List'Class,
             Cursor       => Cursor,
             Return_Type  => Return_Type);
         package Constant_Forward
            renames Constant_Bidirectional.Constant_Forward;
      end Cursors;

      function From_Ref_To_Elem (R : Return_Type) return Element_Type
         is (R.Element.all) with Inline;
      package Cursors_Forward_Convert is
         new Conts.Cursors.Constant_Forward_Convert_Traits
           (Cursors.Constant_Forward, Element_Type, From_Ref_To_Elem);
   end Indefinite_List_Adaptors;

   ----------------------------------------
   -- Adaptor for indefinite hashed maps --
   ----------------------------------------

   generic
      with package Maps is
         new Ada.Containers.Indefinite_Hashed_Maps (<>);
   package Indefinite_Hashed_Maps_Adaptors is
      subtype Element_Type is Maps.Element_Type;
      subtype Return_Type is Maps.Constant_Reference_Type;
      subtype Map is Maps.Map;
      subtype Cursor is Maps.Cursor;

      function First (Self : Map) return Cursor
         renames Maps.First;
      function Element (Self : Map; Position : Cursor) return Return_Type
         is (Maps.Constant_Reference (Self, Position)) with Inline;
      function Has_Element (Self : Map; Position : Cursor) return Boolean
         is (Maps.Has_Element (Position)) with Inline;
      function Next (Self : Map; Position : Cursor) return Cursor
         is (Maps.Next (Position)) with Inline;

      package Cursors is
         package Constant_Forward is new Constant_Forward_Traits
            (Container    => Map'Class,
             Cursor       => Cursor,
             Return_Type  => Return_Type);
      end Cursors;

      function From_Ref_To_Elem (R : Return_Type) return Element_Type
         is (R.Element.all) with Inline;
      package Cursors_Forward_Convert is
         new Conts.Cursors.Constant_Forward_Convert_Traits
           (Cursors.Constant_Forward, Element_Type, From_Ref_To_Elem);
   end Indefinite_Hashed_Maps_Adaptors;

   -----------------------------------------
   -- Adaptor for indefinite ordered maps --
   -----------------------------------------

   generic
      with package Maps is
         new Ada.Containers.Indefinite_Ordered_Maps (<>);
   package Indefinite_Ordered_Maps_Adaptors is
      subtype Element_Type is Maps.Element_Type;
      subtype Return_Type is Maps.Constant_Reference_Type;
      subtype Map is Maps.Map;
      subtype Cursor is Maps.Cursor;

      function First (Self : Map) return Cursor
         renames Maps.First;
      function Element (Self : Map; Position : Cursor) return Return_Type
         is (Maps.Constant_Reference (Self, Position)) with Inline;
      function Has_Element (Self : Map; Position : Cursor) return Boolean
         is (Maps.Has_Element (Position)) with Inline;
      function Next (Self : Map; Position : Cursor) return Cursor
         is (Maps.Next (Position)) with Inline;
      function Previous (Self : Map; Position : Cursor) return Cursor
         is (Maps.Previous (Position)) with Inline;

      package Cursors is
         package Constant_Bidirectional is new Constant_Bidirectional_Traits
            (Container    => Map'Class,
             Cursor       => Cursor,
             Return_Type  => Return_Type);
         package Constant_Forward
            renames Constant_Bidirectional.Constant_Forward;
      end Cursors;

      function From_Ref_To_Elem (R : Return_Type) return Element_Type
         is (R.Element.all) with Inline;
      package Cursors_Forward_Convert is
         new Conts.Cursors.Constant_Forward_Convert_Traits
           (Cursors.Constant_Forward, Element_Type, From_Ref_To_Elem);
   end Indefinite_Ordered_Maps_Adaptors;

   ------------------------
   -- Adaptor for arrays --
   ------------------------

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   package Array_Adaptors is
      function First (Self : Array_Type) return Index_Type
         is (Self'First) with Inline;
      function Element
         (Self : Array_Type; Position : Index_Type) return Element_Type
         is (Self (Position)) with Inline;
      function Has_Element
         (Self : Array_Type; Position : Index_Type) return Boolean
         is (Position <= Self'Last) with Inline;
      function Next
         (Self : Array_Type; Position : Index_Type) return Index_Type
         is (Index_Type'Succ (Position)) with Inline;
      function Previous
         (Self : Array_Type; Position : Index_Type) return Index_Type
         is (Index_Type'Pred (Position)) with Inline;

      package Cursors is
         package Constant_Bidirectional is new Constant_Bidirectional_Traits
            (Container    => Array_Type,
             Cursor       => Index_Type,
             Return_Type  => Element_Type);
         package Constant_Forward
            renames Constant_Bidirectional.Constant_Forward;
      end Cursors;
   end Array_Adaptors;

end Conts.Cursors.Adaptors;
