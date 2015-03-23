--  Common code for all nodes of bounded lists.
--  Such nodes are implemented via an array, so that no dynamic memory
--  allocation is needed

pragma Ada_2012;
with Conts.Generic_Elements;
with Conts.Generic_List_Nodes;

generic
   with package Elements is new Conts.Generic_Elements (<>);
   Capacity : Count_Type;
package Conts.Bounded_List_Nodes is

   subtype Stored_Element_Type is Elements.Stored_Element_Type;

   type Node_Access is new Count_Type;
   Null_Node_Access : constant Node_Access := 0;
   type Node is record
      Element        : Stored_Element_Type;
      Previous, Next : Node_Access := Null_Node_Access;
   end record;

   type Nodes_Array is array (Node_Access range <>) of Node;

   type Nodes_Container is record
      Nodes : Nodes_Array (1 .. Node_Access (Capacity));

      Free  : Integer := 0;   --  head of free nodes list
      --  For a negative value, its absolute value points to the first free
      --  element
   end record;

   procedure Allocate
      (Self    : in out Nodes_Container;
       Element : Stored_Element_Type;
       N       : out Node_Access);
   function Get_Element
      (Self : Nodes_Container; N : Node_Access) return Stored_Element_Type
      is (Self.Nodes (N).Element);
   function Get_Next
      (Self : Nodes_Container; N : Node_Access) return Node_Access
      is (Self.Nodes (N).Next);
   function Get_Previous
      (Self : Nodes_Container; N : Node_Access) return Node_Access
      is (Self.Nodes (N).Previous);
   procedure Set_Next
      (Self : in out Nodes_Container; N, Next : Node_Access);
   procedure Set_Previous
      (Self : in out Nodes_Container; N, Previous : Node_Access);
   pragma Inline (Allocate, Set_Next, Set_Previous);
   pragma Inline (Get_Element, Get_Next, Get_Previous);

   package Nodes is new Conts.Generic_List_Nodes
      (Elements     => Elements,
       Container    => Nodes_Container,
       Node_Access  => Node_Access,
       Null_Access  => Null_Node_Access,
       Allocate     => Allocate);
end Conts.Bounded_List_Nodes;
