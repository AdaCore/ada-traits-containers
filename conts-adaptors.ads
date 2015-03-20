--  This file provides adaptors for the standard Ada2012 containers, so that
--  they can be used with the algorithms declared in our containers hierarchy

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Conts.Adaptors is

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

      package Forward_Cursors is new Forward_Cursors_Traits
         (Container    => List,
          Cursor       => Cursor,
          Element_Type => Element_Type,
          No_Element   => Lists.No_Element);

   end List_Adaptors;

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

      package Forward_Cursors is new Forward_Cursors_Traits
         (Container    => List,
          Cursor       => Cursor,
          Element_Type => Element_Type,
          No_Element   => Lists.No_Element);
   end Indefinite_List_Adaptors;

end Conts.Adaptors;
