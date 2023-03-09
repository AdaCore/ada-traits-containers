--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package describes the underlying storage strategy for a vector.
--  There are mostly two such strategies (bounded and unbounded) depending on
--  whether the vector has a maximal number of elements.

pragma Ada_2012;
with Conts.Elements;

generic
   with package Elements is new Conts.Elements.Traits (<>);
   type Container_Base_Type is abstract tagged limited private;
   with package Resize_Policy is new Conts.Vectors.Resize_Strategy (<>);
package Conts.Vectors.Storage.Unbounded with SPARK_Mode is

   package Impl with SPARK_Mode is
      type Container is abstract new Container_Base_Type with private;

      function Max_Capacity (Self : Container'Class) return Count_Type
         is (Count_Type'Last - Min_Index + 1) with Inline;
      function Capacity (Self : Container'Class) return Count_Type
         with Inline;
      procedure Release_Element
        (Self : in out Container'Class; Index : Count_Type) with Inline;
      function Get_Element
        (Self  : Container'Class;
         Index : Count_Type) return Elements.Stored_Type with Inline;
      procedure Set_Element
        (Self    : in out Container'Class;
         Index   : Count_Type;
         Element : Elements.Stored_Type) with Inline;
      procedure Copy
        (Self                   : in out Container'Class;
         Source                 : Container'Class;
         Source_From, Source_To : Count_Type;
         Self_From              : Count_Type) with Inline;
      procedure Assign
        (Self                : in out Container'Class;
         Source              : Container'Class;
         Last                : Count_Type);
      procedure Resize
        (Self     : in out Container'Class;
         New_Size : Count_Type;
         Last     : Count_Type;
         Force    : Boolean)
        with Pre => New_Size <= Self.Max_Capacity;
      procedure Release (Self : in out Container'Class);

   private
      pragma SPARK_Mode (Off);
      type Big_Nodes_Array is
        array (Min_Index .. Count_Type'Last) of Elements.Stored_Type;
      type Nodes_Array_Access is access Big_Nodes_Array;
      for Nodes_Array_Access'Storage_Size use 0;
      --  The nodes is a C-compatible pointer so that we can use realloc

      type Container is abstract new Container_Base_Type with record
         Nodes : Nodes_Array_Access;

         Capacity : Count_Type := 0;
         --  Last element in Nodes (since Nodes does not contain bounds
         --  information).
      end record;

      function Capacity (Self : Container'Class) return Count_Type
        is (Self.Capacity);
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
      Resize           => Impl.Resize,
      Release_Element  => Impl.Release_Element,
      Release          => Impl.Release,
      Set_Element      => Impl.Set_Element,
      Get_Element      => Impl.Get_Element,
      Assign           => Impl.Assign,
      Copy             => Impl.Copy);

end Conts.Vectors.Storage.Unbounded;
