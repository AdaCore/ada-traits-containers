------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
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
with Ada.Finalization;
with Conts.Elements.Indefinite_Ref;
with Conts.Lists.Nodes.Unbounded;
with Conts.Lists.Generics;

package QGen is

   --  This package checks that an organization as used in the QGen project
   --  is compatible with our proposal.
   --  We have a hierarchy of types, a matching hierarchy of lists, and when
   --  we use a for..of loop on a list, we get the corresponding child type.
   --  We do not want to duplicate the instance of lists, the goal is to
   --  generate minimal additional code since there are hundreds of such
   --  types in QGen.

   type EObject is abstract tagged record
      Id : Integer;
   end record;
   type Block is new EObject with null record;
   type Sum is new Block with null record;

   --  We do our own instances (not the ones in
   --  Conts.Lists.Indefinite_Unbounded) for better sharing of code.

   package Elements is new Conts.Elements.Indefinite_Ref
      (EObject'Class, Pool => Conts.Global_Pool);
   package Nodes is new Conts.Lists.Nodes.Unbounded
      (Elements.Traits,
       Base_Type => Ada.Finalization.Controlled,
       Pool      => Conts.Global_Pool);
   package Lists is new Conts.Lists.Generics (Nodes.Traits);

   type EObject_List is new Lists.List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   type Block_List is new EObject_List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => As_Block);
   function As_Block (C : Block_List; P : Lists.Cursor) return Block'Class
      is (Block'Class (Lists.Element (C, P).E.all))
      with Inline => True;

   type Sum_List is new Block_List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => As_Sum);
   function As_Sum (C : Sum_List; P : Lists.Cursor) return Sum'Class
      is (Sum'Class (Lists.Element (C, P).E.all))
      with Inline => True;

   procedure Test_QGen;

end QGen;
