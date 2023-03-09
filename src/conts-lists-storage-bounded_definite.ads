--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  The implementation for bounded list of definite elements.
--  This implementation does not perform any memory allocation.
--  It is compatible with SPARK.
--
--  As opposed to some of the other list's storage packages, this package does
--  not take a Base_Container formal parameter. This is for compatibility with
--  SPARK, where it is not possible to extend a tagged type with new
--  discriminants.
--  As a result, a bounded list is always non-limited and non-controlled. This
--  only works fine for a list of definite elements where no memory allocation
--  occurs.

pragma Ada_2012;
with Conts.Elements.Definite;

generic
   with package Elements is new Conts.Elements.Definite (<>);
package Conts.Lists.Storage.Bounded_Definite with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Stored_Type is Elements.Traits.Stored;

   package Impl is
      type Container (Capacity : Count_Type) is abstract tagged private;
      type Node_Access is new Count_Type;
      Null_Node_Access : constant Node_Access := 0;

      procedure Allocate
         (Self    : in out Impl.Container'Class;
          Element : Stored_Type;
          N       : out Impl.Node_Access);
      function Get_Element
         (Self : Impl.Container'Class;
          N    : Impl.Node_Access) return Stored_Type with Inline;
      function Get_Next
         (Self : Impl.Container'Class;
          N    : Impl.Node_Access) return Impl.Node_Access with Inline;
      function Get_Previous
         (Self : Impl.Container'Class;
          N    : Impl.Node_Access) return Impl.Node_Access with Inline;
      procedure Set_Previous
         (Self    : in out Impl.Container'Class;
          N, Prev : Impl.Node_Access) with Inline;
      procedure Set_Next
         (Self    : in out Impl.Container'Class;
          N, Next : Impl.Node_Access) with Inline;
      procedure Set_Element
        (Self : in out Impl.Container'Class;
         N    : Node_Access;
         E    : Stored_Type) with Inline;
      function Capacity (Self : Impl.Container'Class) return Count_Type
         is (Self.Capacity) with Inline;
      procedure Assign
         (Nodes    : in out Impl.Container'Class;
          Source   : Impl.Container'Class;
          New_Head : out Impl.Node_Access;
          Old_Head : Impl.Node_Access;
          New_Tail : out Impl.Node_Access;
          Old_Tail : Impl.Node_Access)
        with Pre => Nodes.Capacity >= Source.Capacity;
      --  See description in Conts.Lists.Nodes

   private
      pragma SPARK_Mode (Off);

      type Node is record
         Element        : Stored_Type;
         Previous, Next : Node_Access := Null_Node_Access;
      end record;

      type Nodes_Array is array (Count_Type range <>) of Node;

      type Container (Capacity : Count_Type) is abstract tagged record
         Free  : Integer := 0;   --  head of free nodes list
         --  For a negative value, its absolute value points to the first free
         --  element

         Nodes : Nodes_Array (1 .. Capacity);
      end record;
   end Impl;

   use Impl;
   package Traits is new Conts.Lists.Storage.Traits
      (Elements     => Elements.Traits,
       Container    => Impl.Container,
       Node_Access  => Impl.Node_Access,
       Null_Access  => Impl.Null_Node_Access,
       Allocate     => Allocate);
end Conts.Lists.Storage.Bounded_Definite;
