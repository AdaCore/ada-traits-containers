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

--  This package provides a specialization of the Element_Traits package for
--  use with indefinite type (i.e. their size might not be known at compile
--  time).
--  Such elements are returned by copy. It might be more efficient to use
--  conts-elements-indefinite_ref.ads instead to return the elements by
--  reference.

pragma Ada_2012;

generic
   type Element_Type (<>) is private;

   with procedure Free (E : in out Element_Type) is null;
   --  This procedure is called when the element is removed from its
   --  container.

   with package Pool is new Conts.Pools (<>);

package Conts.Elements.Indefinite with SPARK_Mode is

   type Element_Access is access all Element_Type;
   for Element_Access'Storage_Pool use Pool.Pool.all;

   function To_Element_Access (E : Element_Type) return Element_Access
      is (new Element_Type'(E)) with Inline;
   function To_Element (E : Element_Access) return Element_Type
      is (E.all) with Inline;
   function Copy (E : Element_Access) return Element_Access
      is (new Element_Type'(E.all)) with Inline;
   procedure Release (E : in out Element_Access) with Inline;

   package Traits is new Conts.Elements.Traits
      (Element_Type        => Element_Type,
       Stored_Type         => Element_Access,
       Return_Type         => Element_Type,
       To_Stored           => To_Element_Access,
       To_Return           => To_Element,
       Copy                => Copy,
       Release             => Release,
       Copyable            => False,   --  would create aliases
       Movable             => True);

end Conts.Elements.Indefinite;
