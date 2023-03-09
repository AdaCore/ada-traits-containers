--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

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
