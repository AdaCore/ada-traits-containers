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

--  The implementation for bounded list of elements.
--  Such a list allocates its nodes in an array, so that no memory allocation
--  is needed. The implementation therefore looks like a vector, but since we
--  need to be able to insert in the middle of the list in constant time, the
--  nodes need to store extra information.

pragma Ada_2012;
with Conts.Elements;

generic
   with package Elements is new Conts.Elements.Traits (<>);

   type Container_Base_Type is abstract tagged limited private;
   --  The base type for the container of nodes.
   --  Since this type is eventually also used as the base type for the list
   --  itself, this is a way to make lists either controlled or limited.

package Conts.Lists.Storage.Bounded with SPARK_Mode => Off is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Stored_Type is Elements.Stored_Type;

   package Impl is
      type Container (Capacity : Count_Type)
         is abstract new Container_Base_Type with private;
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
      type Node is record
         Element        : Stored_Type;
         Previous, Next : Node_Access := Null_Node_Access;
      end record;

      type Nodes_Array is array (Count_Type range <>) of Node;

      type Container (Capacity : Count_Type) is
         abstract new Container_Base_Type
      with record
         Free  : Integer := 0;   --  head of free nodes list
         --  For a negative value, its absolute value points to the first free
         --  element

         Nodes : Nodes_Array (1 .. Capacity);
      end record;
   end Impl;

   use Impl;
   package Traits is new Conts.Lists.Storage.Traits
      (Elements     => Elements,
       Container    => Impl.Container,
       Node_Access  => Impl.Node_Access,
       Null_Access  => Impl.Null_Node_Access,
       Allocate     => Allocate);
end Conts.Lists.Storage.Bounded;
