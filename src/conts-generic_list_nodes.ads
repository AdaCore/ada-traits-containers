--  This package describes nodes for list containers
--  Such nodes can be implemented via different ways, for instance standard
--  access types (see Conts.Unbounded_List_Nodes), an array of nodes with no
--  memory allocation, or as an adaptor for an existing data structure

--  This concept of setting and getting values is what is called a
--  'property map' in Boost Graph, and a is way to abstract whether the
--  information is stored in a node, or externally.
--  If the getters and setters are marked as inline, the cost of using
--  an indirection via a generic package is negligible.

pragma Ada_2012;
with Conts.Generic_Elements;

generic
   with package Elements is new Conts.Generic_Elements (<>);
   --  The type of elements stored in nodes
  
   type Container is private;
   --  A container for all nodes (if applicable, otherwise it could be a
   --  null record when nodes are allocated on the heap via malloc).

   type Node_Access is private;
   --  Access to a node. This is either an actual pointer or an index into
   --  some other data structure.

   Null_Access : Node_Access;
   --  ??? We could use a Is_Valid function instead, might be more general

   with procedure Allocate
      (Nodes : in out Container;
       Element  : Elements.Stored_Element_Type;
       New_Node : out Node_Access);
   --  Allocate a new node, that contains Element. Its next and previous
   --  siblings have been initialized to Null_Access.
   --  This procedure can return Null_Access is the new node could not be
   --  allocated.

   with function Get_Element
      (Nodes    : Container;
       Position : Node_Access) return Elements.Stored_Element_Type is <>;
   with function Get_Next
      (Nodes    : Container;
       Position : Node_Access) return Node_Access is <>;
   with function Get_Previous
      (Nodes    : Container;
       Position : Node_Access) return Node_Access is <>;
   --  Get the next and previous elements for a node

   with procedure Set_Next
      (Nodes    : in out Container;
       Position : Node_Access;
       Next     : Node_Access) is <>;
   with procedure Set_Previous
      (Nodes    : in out Container;
       Position : Node_Access;
       Previous : Node_Access) is <>;
   --  Change the next and previous elements for a node

package Conts.Generic_List_Nodes is
   --  pragma Unreferenced (Null_Access, Allocate, Get_Element, Get_Next);
   --  pragma Unreferenced (Get_Previous, Set_Next, Set_Previous);
   --  ??? Other packages need those, but the compiler is complaining that
   --  these formal parameters are unused in this package.
end Conts.Generic_List_Nodes;

