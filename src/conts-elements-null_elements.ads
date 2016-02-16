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

--  A special kind of elements that store nothing.
--  This is only useful to instantiate some containers, for instance a graph,
--  when no extra information needs to be added to the vertices.

pragma Ada_2012;

package Conts.Elements.Null_Elements is

   type Null_Element is null record;

   No_Element : constant Null_Element := (others => <>);

   function Identity (E : Null_Element) return Null_Element is (E) with Inline;
   package Traits is new Conts.Elements.Traits
     (Element_Type            => Null_Element,
      Stored_Type             => Null_Element,
      Returned_Type           => Null_Element,
      Constant_Returned_Type  => Null_Element,
      Copyable                => True,
      Movable                 => True,
      To_Stored               => Identity,
      To_Returned             => Identity,
      To_Constant_Returned    => Identity,
      To_Element              => Identity,
      Copy                    => Identity);

end Conts.Elements.Null_Elements;
