package Conts is

   subtype Count_Type is Natural;

   generic
      type Container (<>) is limited private;
      type Cursor is private;
      type Element_Type (<>) is private;
      with function First (Self : Container) return Cursor is <>;
      with function Element (Self : Container; Position : Cursor)
         return Element_Type is <>;
      with function Has_Element (Self : Container; Position : Cursor)
         return Boolean is <>;
      with function Next (Self : Container; Position : Cursor)
         return Cursor is <>;
   package Forward_Cursors_Traits is
   end Forward_Cursors_Traits;
   --  A package that describes how to use forward cursors.
   --  Each contain for which this is applicable provides an instance of
   --  this package, and algorithms should take this package as a
   --  generic parameter.

end Conts;
