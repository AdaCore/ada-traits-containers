--  Common code for all nodes of unbounded lists.
--  Such nodes are implemented via standard access types

pragma Ada_2012;
with Conts.Generic_List_Nodes;

generic
   with package Elements is new Element_Traits (<>);
package Conts.Unbounded_List_Nodes is

   subtype Stored_Element_Type is Elements.Stored_Element_Type;

   type Nodes_Container is null record;
   type Node;
   type Node_Access is access Node;
   type Node is record
      Element        : Stored_Element_Type;
      Previous, Next : Node_Access;
   end record;
   procedure Allocate
      (Self    : in out Nodes_Container;
       Element : Stored_Element_Type;
       N       : out Node_Access);
   function Get_Element
      (Self : Nodes_Container; N : Node_Access) return Stored_Element_Type
      is (N.Element);
   function Get_Next
      (Self : Nodes_Container; N : Node_Access) return Node_Access
      is (N.Next);
   function Get_Previous
      (Self : Nodes_Container; N : Node_Access) return Node_Access
      is (N.Previous);
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
       Null_Access  => null,
       Allocate     => Allocate);
end Conts.Unbounded_List_Nodes;
