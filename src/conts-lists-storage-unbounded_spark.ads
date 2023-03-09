--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides support for unbounded list, compatible with SPARK.
--  It uses an resizable array of access types. It is resizable so that there
--  is no upper-limit on the size of the list. Since it is an array, cursors
--  are indexes into this array, not actual access types, so that we can write
--  pre and post-conditions conveniently.

pragma Ada_2012;
with Conts.Elements;

generic
   with package Elements is new Conts.Elements.Traits (<>);

   type Container_Base_Type is abstract tagged limited private;
   --  The base type for these unbounded lists

package Conts.Lists.Storage.Unbounded_SPARK with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   type Node_Access is new Count_Type;
   Null_Node_Access : constant Node_Access := 0;
   type Node is record
      Element        : Elements.Stored_Type;
      Previous, Next : Node_Access := Null_Node_Access;
   end record;

   type Big_Nodes_Array is array (1 .. Count_Type'Last) of Node;

   package Private_Nodes_List with SPARK_Mode is
      type Nodes_List is abstract new Container_Base_Type with private;

      procedure Allocate
        (Self    : in out Nodes_List'Class;
         Element : Elements.Stored_Type;
         N       : out Node_Access);   --  not inlined
      procedure Release (Self : in out Nodes_List'Class);
      function Get_Element
        (Self : Nodes_List'Class; N : Node_Access)
        return Elements.Stored_Type
        with Inline;
      function Get_Next
        (Self : Nodes_List'Class; N : Node_Access) return Node_Access
        with Inline;
      function Get_Previous
        (Self : Nodes_List'Class; N : Node_Access) return Node_Access
        with Inline;
      procedure Set_Next
        (Self : in out Nodes_List'Class; N, Next : Node_Access)
        with Inline;
      procedure Set_Previous
        (Self : in out Nodes_List'Class; N, Previous : Node_Access)
        with Inline;
      procedure Set_Element
        (Self : in out Nodes_List'Class;
         N    : Node_Access;
         E    : Elements.Stored_Type) with Inline;
      function Capacity (Self : Nodes_List'Class) return Count_Type
        is (Count_Type'Last) with Inline;
      procedure Assign
        (Nodes    : in out Nodes_List'Class;
         Source   : Nodes_List'Class;
         New_Head : out Node_Access;
         Old_Head : Node_Access;
         New_Tail : out Node_Access;
         Old_Tail : Node_Access);
   private
      pragma SPARK_Mode (Off);
      type Nodes_Array_Access is access Big_Nodes_Array;
      for Nodes_Array_Access'Storage_Size use 0;
      --  The nodes is a pointer so that we can use realloc

      type Nodes_List is abstract new Container_Base_Type with record
         Nodes : Nodes_Array_Access := null;
         Last  : Count_Type := 0;  --  Last valid index in Nodes
         Free  : Integer := 0;     --  head of free nodes list
         --  For a negative value, its absolute value points to the first
         --  free element
      end record;

      function Get_Element
        (Self : Nodes_List'Class; N : Node_Access)
       return Elements.Stored_Type
      is (Self.Nodes (Count_Type (N)).Element);
      function Get_Next
        (Self : Nodes_List'Class; N : Node_Access) return Node_Access
      is (Self.Nodes (Count_Type (N)).Next);
      function Get_Previous
        (Self : Nodes_List'Class; N : Node_Access) return Node_Access
      is (Self.Nodes (Count_Type (N)).Previous);
   end Private_Nodes_List;

   use Private_Nodes_List;

   package Traits is new Conts.Lists.Storage.Traits
     (Elements     => Elements,
      Container    => Nodes_List,
      Node_Access  => Node_Access,
      Null_Access  => Null_Node_Access,
      Allocate     => Allocate,
      Release      => Release);

end Conts.Lists.Storage.Unbounded_SPARK;
