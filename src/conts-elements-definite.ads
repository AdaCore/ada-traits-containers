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

--  This unit provides a specialization of the element traits, for use with
--  definite elements, i.e. elements whose size is known at compile time.
--  Such elements do not need an extra level of indirection via pointers to
--  be stored in a container.

pragma Ada_2012;

generic
   type Element_Type is private;

   with procedure Free (E : in out Element_Type) is null;
   --  Free is called when the element is no longer used (removed from
   --  its container for instance). Most of the time this will do
   --  nothing, but this procedure is useful if the Element_Type is an
   --  access type that you want to deallocate.

   Copyable : Boolean := False;   --  should be False for access types
   Movable  : Boolean := True;    --  should be False for controlled types

package Conts.Elements.Definite with SPARK_Mode is

   function Identity (E : Element_Type) return Element_Type is (E) with Inline;

   package Traits is new Conts.Elements.Traits
      (Element_Type  => Element_Type,
       Stored_Type   => Element_Type,
       Return_Type   => Element_Type,
       Copyable      => Copyable,
       Movable       => Movable,
       Release       => Free,
       To_Stored     => Identity,
       To_Return     => Identity,
       Copy          => Identity);

end Conts.Elements.Definite;
