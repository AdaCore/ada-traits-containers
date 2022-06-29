--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package describes the underlying storage strategy for a bounded vector

pragma Ada_2012;
with Conts.Elements;

generic
   with package Elements is new Conts.Elements.Traits (<>);

   type Container_Base_Type is abstract tagged limited private;
   --  The base type for the container of nodes.
   --  Since this type is eventually also used as the base type for the list
   --  itself, this is a way to make lists either controlled or limited.

package Conts.Vectors.Storage.Bounded with SPARK_Mode is

   package Impl is
      type Container (Capacity : Count_Type)
         is abstract new Container_Base_Type with private;

      function Max_Capacity (Self : Container'Class) return Count_Type
         is (Self.Capacity) with Inline;
      function Capacity (Self : Container'Class) return Count_Type
         is (Self.Capacity) with Inline;
      procedure Release_Element
        (Self : in out Container'Class; Index : Count_Type) with Inline;
      procedure Set_Element
        (Self    : in out Container'Class;
         Index   : Count_Type;
         Element : Elements.Stored_Type) with Inline;
      function Get_Element
        (Self  : Container'Class;
         Index : Count_Type) return Elements.Stored_Type with Inline;
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
      type Elem_Array is array (Count_Type range <>) of Elements.Stored_Type;

      type Container (Capacity : Count_Type) is
         abstract new Container_Base_Type
      with record
         Nodes : Elem_Array (Min_Index .. Capacity);
      end record;

      function Get_Element
        (Self  : Container'Class;
         Index : Count_Type) return Elements.Stored_Type
         is (Self.Nodes (Index));
   end Impl;

   package Traits is new Conts.Vectors.Storage.Traits
     (Elements         => Elements,
      Container        => Impl.Container,
      Max_Capacity     => Impl.Max_Capacity,
      Capacity         => Impl.Capacity,
      Release_Element  => Impl.Release_Element,
      Set_Element      => Impl.Set_Element,
      Get_Element      => Impl.Get_Element,
      Assign           => Impl.Assign,
      Copy             => Impl.Copy);

end Conts.Vectors.Storage.Bounded;
