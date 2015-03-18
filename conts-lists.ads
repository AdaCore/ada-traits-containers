pragma Ada_2012;
with Conts.Lists_Impl;

generic
   type Element_Type is private;

   Enable_Asserts : Boolean := False;
   --  If True, extra asserts are added to the code. Apart from them, this
   --  code runs with all compiler checks disabled.

package Conts.Lists is
   package Lists is new Conts.Lists_Impl
      (Element_Type   => Element_Type,
       Enable_Asserts => Enable_Asserts);
   use Lists;
   --  The actual types are defined in a different package, so that they are
   --  already frozen when we declare the Cursors packages below.

   subtype List is Lists.List;
   subtype Cursor is Lists.Cursor;
   No_Element : constant Cursor := Lists.No_Element;

   procedure Append
      (Self : in out List; Element : Element_Type) renames Lists.Append;
   function Length (Self : List) return Count_Type renames Lists.Length;
   function First (Self : List) return Cursor renames Lists.First;
   function Element (Self : List; Position : Cursor) return Element_Type
      renames Lists.Element;
   function Has_Element (Self : List; Position : Cursor) return Boolean
      renames Lists.Has_Element;
   function Next (Self : List; Position : Cursor) return Cursor
      renames Lists.Next;
   --  Renames for all the subprograms in Lists, for people that do not use
   --  the Ada2012 notation for primitive operations.

   package Forward_Cursors is new Forward_Cursors_Traits
      (Container    => List,
       Cursor       => Cursor,
       Element_Type => Element_Type,
       No_Element   => No_Element);
end Conts.Lists;
