--  Common code for all nodes of bounded lists.
--  Such nodes are implemented via an array, so that no dynamic memory
--  allocation is needed

pragma Ada_2012;

package body Conts.Bounded_List_Nodes is

   --------------
   -- Allocate --
   --------------

   procedure Allocate
      (Self    : in out Nodes_Container;
       Element : Stored_Element_Type;
       N       : out Node_Access)
   is
      --  N : Node_Array renames Container.Nodes;

   begin
      if Self.Free > 0 then
         N := Node_Access (Self.Free);
         Self.Free := Integer (Self.Nodes (N).Next);
      else
         N := Node_Access (abs Self.Free + 1);
         Self.Free := Self.Free - 1;
      end if;

      Self.Nodes (N) := 
         (Element  => Element,
          Previous => Null_Node_Access,
          Next     => Null_Node_Access);
   end Allocate;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
      (Self : in out Nodes_Container; N, Next : Node_Access) is
   begin
      Self.Nodes (N).Next := Next;
   end Set_Next;

   ------------------
   -- Set_Previous --
   ------------------

   procedure Set_Previous
      (Self : in out Nodes_Container; N, Previous : Node_Access) is
   begin
      Self.Nodes (N).Previous := Previous;
   end Set_Previous;

end Conts.Bounded_List_Nodes;
