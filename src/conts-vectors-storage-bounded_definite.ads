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

--  The implementation for bounded vectors of definite elements.
--  This implementation does not perform any memory allocation.
--  It is compatible with SPARK.

pragma Ada_2012;
with Conts.Elements.Definite;

generic
   with package Elements is new Conts.Elements.Definite (<>);
package Conts.Vectors.Storage.Bounded_Definite with SPARK_Mode is

   subtype Stored_Type is Elements.Traits.Stored;

   package Impl is
      type Container (Capacity : Count_Type) is abstract tagged private;

      function Max_Capacity (Self : Container'Class) return Count_Type
         is (Self.Capacity) with Inline;
      function Capacity (Self : Container'Class) return Count_Type
         is (Self.Capacity) with Inline;
      procedure Set_Element
        (Self    : in out Container'Class;
         Index   : Count_Type;
         Element : Stored_Type) with Inline;
      function Get_Element
        (Self  : Container'Class;
         Index : Count_Type) return Stored_Type with Inline;
      procedure Assign
        (Self                : in out Container'Class;
         Source              : Container'Class;
         Last                : Count_Type);
      procedure Copy
        (Self                   : in out Container'Class;
         Source                 : Container'Class;
         Source_From, Source_To : Count_Type;
         Self_From              : Count_Type) with Inline;

   private
      pragma SPARK_Mode (Off);
      type Elem_Array is array (Count_Type range <>) of Stored_Type;

      type Container (Capacity : Count_Type) is abstract tagged record
         Nodes : Elem_Array (Min_Index .. Capacity);
      end record;

      function Get_Element
        (Self  : Container'Class;
         Index : Count_Type) return Stored_Type
         is (Self.Nodes (Index));
   end Impl;

   package Traits is new Conts.Vectors.Storage.Traits
     (Elements         => Elements.Traits,
      Container        => Impl.Container,
      Max_Capacity     => Impl.Max_Capacity,
      Capacity         => Impl.Capacity,
      Set_Element      => Impl.Set_Element,
      Get_Element      => Impl.Get_Element,
      Assign           => Impl.Assign,
      Copy             => Impl.Copy);

end Conts.Vectors.Storage.Bounded_Definite;
