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

pragma Ada_2012;
with Conts.Dicts.Strings; use Conts.Dicts.Strings;

package Conts.Dicts.Traits is

   package String_Traits is new Elements_Traits
     (Element_Type        => String,
      Stored_Element_Type => Stored_String,
      Reference_Type      => String, -- ??? how to implement this
      Convert_From        => Convert_From,
      Convert_To          => Convert_To,
      Get_Reference       => Get_Reference,
      Release             => Release,
      Use_Implicit_Copy   => False);

   generic
      type Value_Type is private;
      with procedure Free (E : in out Value_Type) is null;

   package Definite_String_Dict_Traits is

      package Value_Traits is new Definite_Elements_Traits (Value_Type, Free);

      package Traits is new Conts.Dicts.Dict_Traits
        (Unsigned_32,
         String_Traits,
         Value_Traits.Elements,
         Hash => Hash,
         Equals => "=",
         Container_Size => Dynamic_Container_Size);
   end Definite_String_Dict_Traits;

   generic
      type Value_Type is private;
      with procedure Free (E : in out Value_Type) is null;

   package Indefinite_String_Dict_Traits is

      package Value_Traits
         is new Indefinite_Elements_Traits (Value_Type, Free);

      package Traits is new Conts.Dicts.Dict_Traits
        (Unsigned_32,
         String_Traits,
         Value_Traits.Elements,
         Hash => Hash,
         Equals => "=",
         Container_Size => Dynamic_Container_Size);

   end Indefinite_String_Dict_Traits;

end Conts.Dicts.Traits;
