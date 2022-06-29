--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides support for unbounded lists.
--  All nodes are allocated on the heap.

pragma Ada_2012;
with Conts.Elements;

generic
   with package Elements is new Conts.Elements.Traits (<>);

   type Container_Base_Type is abstract tagged limited private;
   --  The base type for these unbounded list.

   with package Pool is new Conts.Pools (<>);
   --  The storage pool used for nodes.

package Conts.Lists.Storage.Unbounded with SPARK_Mode => Off is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Nodes_Container is Container_Base_Type;
   type Node;
   type Node_Access is access Node;
   for Node_Access'Storage_Pool use Pool.Pool;

   --  ??? Compiler crashes if we make this type private
   type Node is record
      Element        : Elements.Stored_Type;
      Previous, Next : Node_Access;
   end record;

   procedure Allocate
      (Self    : in out Nodes_Container'Class;
       Element : Elements.Stored_Type;
       N       : out Node_Access)
      with Inline;
   procedure Release_Node
      (Self : in out Nodes_Container'Class; N : in out Node_Access);
   function Get_Element
      (Self : Nodes_Container'Class; N : Node_Access)
      return Elements.Stored_Type
      with Inline;
   function Get_Next
      (Self : Nodes_Container'Class; N : Node_Access) return Node_Access
      with Inline;
   function Get_Previous
      (Self : Nodes_Container'Class; N : Node_Access) return Node_Access
      with Inline;
   procedure Set_Previous
      (Self : in out Nodes_Container'Class; N, Previous : Node_Access)
      with Inline;
   procedure Set_Next
      (Self : in out Nodes_Container'Class; N, Next : Node_Access)
      with Inline;
   procedure Set_Element
     (Self : in out Nodes_Container'Class;
      N    : Node_Access;
      E    : Elements.Stored_Type)
     with Inline;
   function Capacity (Self : Nodes_Container'Class) return Count_Type
      is (Count_Type'Last) with Inline;
   procedure Assign
      (Nodes    : in out Nodes_Container'Class;
       Source   : Nodes_Container'Class;
       New_Head : out Node_Access;
       Old_Head : Node_Access;
       New_Tail : out Node_Access;
       Old_Tail : Node_Access);

   package Traits is new Conts.Lists.Storage.Traits
      (Elements       => Elements,
       Container      => Nodes_Container,
       Node_Access    => Node_Access,
       Null_Access    => null,
       Allocate       => Allocate,
       Release_Node   => Release_Node);

end Conts.Lists.Storage.Unbounded;
