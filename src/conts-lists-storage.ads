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

--  The following packages are used to describe some types of nodes that can be
--  used to build a list. We use a different type depending on whether we have
--  a bounded or unbounded list, for instance. Other implementations are
--  possible to adapt to existing data structures, for instance.

pragma Ada_2012;
with Conts.Elements;

package Conts.Lists.Storage with SPARK_Mode is

   generic
      with package Elements is new Conts.Elements.Traits (<>);
      --  The type of elements stored in nodes

      type Container (<>) is abstract tagged limited private;
      --  A container for all nodes.
      --  Such a container is not needed when nodes are allocated on the heap
      --  and accessed via pointers; but it is needed when nodes are stored in
      --  an array, for instance.
      --  This is used as the ancestor type for the list types, so that this
      --  type can actually be an unconstrained type (which we could not store
      --  inside another list).

      type Node_Access is private;
      --  Access to a node. This is either an actual pointer or an index into
      --  some other data structure.

      Null_Access : Node_Access;

      with procedure Allocate
         (Self     : in out Container'Class;
          Element  : Elements.Stored_Type;
          New_Node : out Node_Access);
      --  Allocate a new node, that contains Element. Its next and previous
      --  siblings have been initialized to Null_Access.
      --  This procedure can return Null_Access is the new node could not be
      --  allocated. This should only happen when there is more than Capacity
      --  elements in Self.

      with procedure Release_Node
         (Self : in out Container'Class; N : in out Node_Access) is null;
      --  Free the memory for a specific node.
      --  This function should not free the element itself, this has already
      --  been handled by the container (this is so that a null procedure can
      --  be passed in the common case).

      with procedure Release (Self : in out Container'Class) is null;
      --  Free all the memory used by the container.
      --  This should not free the nodes themselves, this has already been
      --  taken care of by the container. This is so that a null procedure
      --  can be passed in the common case.

      with function Get_Element
         (Self : Container'Class;
          Pos  : Node_Access) return Elements.Stored_Type is <>;
      with function Get_Next
         (Self : Container'Class; Pos  : Node_Access) return Node_Access is <>;
      with function Get_Previous
         (Self : Container'Class; Pos  : Node_Access) return Node_Access is <>;
      --  Get the next and previous elements for a node
      --  Must return Null_Access when there is no such element.

      with procedure Set_Element
        (Self     : in out Container'Class;
         Pos      : Node_Access;
         Element  : Elements.Stored_Type) is <>;
      --  Replace the element at the given position.
      --  This does not free the previous element.

      with procedure Set_Previous
         (Self     : in out Container'Class;
          Pos      : Node_Access;
          Previous : Node_Access) is <>;
      with procedure Set_Next
         (Self     : in out Container'Class;
          Pos      : Node_Access;
          Next     : Node_Access) is <>;
      --  Change the next and previous elements for a node

      with function Capacity (Self : Container'Class) return Count_Type is <>;
      --  How many nodes can be stored in Nodes

      with procedure Assign
         (Self     : in out Container'Class;
          Source   : Container'Class;
          New_Head : out Node_Access;
          Old_Head : Node_Access;
          New_Tail : out Node_Access;
          Old_Tail : Node_Access) is <>;
      --  Replace all nodes in Nodes with a copy of the nodes in Source.
      --  The elements themselves need to be copied (via Elements.Copy).

   package Traits with SPARK_Mode is
   end Traits;

end Conts.Lists.Storage;
