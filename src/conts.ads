pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package Conts is

   subtype Count_Type is Natural;

   --------------
   -- Elements --
   --------------
   --  This package describes the types of elements stored in a container.
   --  We want to handle both constrained and unconstrained elements, which is
   --  done by providing subprograms to convert from one type to the other
   --  (presumably, but not limited to, using access types)
   
   generic
      type Element_Type (<>) is private;
      --  The element type visible to the user (in parameter to Append, and
      --  returned by Element, for instance)
   
      type Stored_Element_Type is private;
      --  The type of elements stored internally. This must be unconstrained.
   
      with function Convert_From (E : Element_Type) return Stored_Element_Type;
      with function Convert_To (E : Stored_Element_Type) return Element_Type;
      --  Converting between the two types
   
      with procedure Release (E : in out Stored_Element_Type) is null;
      --  Called whenever a value of Element_Type is removed from the table.
      --  This can be used to add unconstrained types to the list: Element_Type
      --  would then be a pointer to that type, and Release is a call to
      --  Unchecked_Deallocation.
      
   package Element_Traits is
      --  pragma Unreferenced (Convert_From, Convert_To, Release);
      --  Can't use the pragma above since other packages need those. But then
      --  the compiler is complaining that these formal parameters are unused
   end Element_Traits;

   -------------------------------------
   -- Definite (constrained) elements --
   -------------------------------------

   generic
      type Element_Type is private;
   package Definite_Element_Traits is
      function Identity (E : Element_Type) return Element_Type is (E);
      pragma Inline (Identity);
      package Elements is new Element_Traits
         (Element_Type        => Element_Type,
          Stored_Element_Type => Element_Type,
          Convert_From        => Identity,
          Convert_To          => Identity);
   end Definite_Element_Traits;

   -----------------------------------------
   -- Indefinite (unconstrained) elements --
   -----------------------------------------

   generic
      type Element_Type (<>) is private;
   package Indefinite_Element_Traits is
      type Element_Access is access all Element_Type;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Element_Type, Element_Access);
      function To_Element_Access (E : Element_Type) return Element_Access
         is (new Element_Type'(E));
      function To_Element_Type (E : Element_Access) return Element_Type
         is (E.all);
      pragma Inline (To_Element_Access, To_Element_Type);

      package Elements is new Element_Traits
         (Element_Type        => Element_Type,
          Stored_Element_Type => Element_Access,
          Convert_From        => To_Element_Access,
          Convert_To          => To_Element_Type,
          Release             => Unchecked_Free);
   end Indefinite_Element_Traits;

   -------------
   -- Cursors --
   -------------

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
      with function Previous (Self : Container; Position : Cursor)
         return Cursor is <>;
   package Bidirectional_Cursors_Traits is

      --  A bidirectional cursor is also a forward cursor
      package Forward_Cursors is new Forward_Cursors_Traits
         (Container, Cursor, Element_Type);
   end Bidirectional_Cursors_Traits;

end Conts;
