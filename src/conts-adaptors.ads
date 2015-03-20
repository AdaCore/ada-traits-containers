--  This file provides adaptors for the standard Ada2012 containers, so that
--  they can be used with the algorithms declared in our containers hierarchy

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Conts.Adaptors is

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
         is (Lists.Element (Position));
      function Has_Element (Self : List; Position : Cursor) return Boolean
         is (Lists.Has_Element (Position));
      function Next (Self : List; Position : Cursor) return Cursor
         is (Lists.Next (Position));
      pragma Inline (Element, Has_Element, Next, First);

      package Forward_Cursors is new Forward_Cursors_Traits
         (Container    => List,
          Cursor       => Cursor,
          Element_Type => Element_Type);
   end List_Adaptors;

   ------------------------------------------------
   -- Adaptor for indefinite doubly linked lists --
   ------------------------------------------------

   generic
      with package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (<>);
   package Indefinite_List_Adaptors is
      subtype Element_Type is Lists.Element_Type;
      subtype List is Lists.List;
      subtype Cursor is Lists.Cursor;

      function First (Self : List) return Cursor
         renames Lists.First;
      function Element (Self : List; Position : Cursor) return Element_Type
         is (Lists.Element (Position));
      function Has_Element (Self : List; Position : Cursor) return Boolean
         is (Lists.Has_Element (Position));
      function Next (Self : List; Position : Cursor) return Cursor
         is (Lists.Next (Position));
      pragma Inline (Element, Has_Element, Next, First);

      package Forward_Cursors is new Forward_Cursors_Traits
         (Container    => List,
          Cursor       => Cursor,
          Element_Type => Element_Type);
   end Indefinite_List_Adaptors;

   ------------------------
   -- Adaptor for arrays --
   ------------------------

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   package Array_Adaptors is
      function First (Self : Array_Type) return Index_Type is (Self'First);
      function Element
         (Self : Array_Type; Position : Index_Type) return Element_Type
         is (Self (Position));
      function Has_Element
         (Self : Array_Type; Position : Index_Type) return Boolean
         is (Position in Self'Range);
      function Next
         (Self : Array_Type; Position : Index_Type) return Index_Type
         is (Index_Type'Succ (Position));
      pragma Inline (Element, Has_Element, Next, First);

      package Forward_Cursors is new Forward_Cursors_Traits
         (Container    => Array_Type,
          Cursor       => Index_Type,
          Element_Type => Element_Type);
   end Array_Adaptors;

end Conts.Adaptors;
