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

--  An implementation of the elements traits compatible with SPARK.
--  This package hides the access types.

pragma Ada_2012;

generic
   type Element_Type (<>) is private;

   with package Pool is new Conts.Pools (<>);
   --  The storage pool used for Elements.

package Conts.Elements.Indefinite_SPARK with SPARK_Mode => On is

   package Impl with SPARK_Mode => On is
      type Element_Access is private;
      function To_Element_Access (E : Element_Type) return Element_Access
         with Inline;
      function To_Element_Type (E : Element_Access) return Element_Type
         with Inline;
      function Identity (E : Element_Type) return Element_Type
         is (E) with Inline;
      function Copy (E : Element_Access) return Element_Access with Inline;
      procedure Free (X : in out Element_Access);
   private
      pragma SPARK_Mode (Off);
      type Element_Access is access all Element_Type;
      for Element_Access'Storage_Pool use Pool.Pool.all;
      function To_Element_Access (E : Element_Type) return Element_Access
         is (new Element_Type'(E));
      function To_Element_Type (E : Element_Access) return Element_Type
         is (E.all);
      function Copy (E : Element_Access) return Element_Access
         is (new Element_Type'(E.all));
   end Impl;

   package Traits is new Conts.Elements.Traits
      (Element_Type        => Element_Type,
       Stored_Type         => Impl.Element_Access,
       Return_Type         => Element_Type,
       To_Stored           => Impl.To_Element_Access,
       To_Return           => Impl.To_Element_Type,
       To_Element          => Impl.Identity,
       Copy                => Impl.Copy,
       Copyable            => False,
       Movable             => False,
       Release             => Impl.Free);

end Conts.Elements.Indefinite_SPARK;
