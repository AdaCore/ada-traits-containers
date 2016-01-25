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

--  Unbounded Vectors of unconstrained elements

pragma Ada_2012;
with Ada.Finalization;
with Conts.Cursors;
with Conts.Elements.Indefinite_Ref;
with Conts.Vectors.Generics;
with Conts.Vectors.Cursors;
with Conts.Vectors.Nodes.Unbounded;

generic
   type Index_Type is range <>;
   type Element_Type (<>) is private;
package Conts.Vectors.Indefinite_Unbounded_Ref is

   package Elements is new Conts.Elements.Indefinite_Ref
      (Element_Type, Pool => Conts.Global_Pool);
   package Nodes is new Conts.Vectors.Nodes.Unbounded
      (Elements      => Elements.Traits,
       Base_Type     => Ada.Finalization.Controlled,
       Resize_Policy => Conts.Vectors.Resize_1_5);
   package Vectors is new Conts.Vectors.Generics (Index_Type, Nodes.Traits);

   subtype Cursor is Vectors.Cursor;
   type Vector is new Vectors.Vector with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   --  ??? Should we provide a Copy function ?
   --  This cannot be provided in the generic package, since the type could
   --  be constrained and/or limited, so it has to be provided in all child
   --  packages. However, when the type is controlled it is much easier to
   --  just use the standard assignment operator.

   package Cursors is new Conts.Vectors.Cursors (Vectors, Vector);

   package Cursors_Forward_Convert
      is new Conts.Cursors.Constant_Forward_Convert_Traits
         (Cursors      => Cursors.Constant_Forward,
          Element_Type => Element_Type,
          Convert      => Elements.From_Ref_To_Element);
   --  A special wrapper around cursor, for use with algorithms, so that
   --  the predicates can take an element_type in parameter

end Conts.Vectors.Indefinite_Unbounded_Ref;
