--  Unbounded lists of constrained elements

pragma Ada_2012;

package body Conts.Unbounded_List_Nodes is

   procedure Allocate
      (Self    : in out Nodes_Container;
       Element : Stored_Element_Type;
       N       : out Node_Access)
   is
      pragma Unreferenced (Self);
   begin
      N := new Node;
      if N /= null then
         N.Element := Element;
      end if;
   end Allocate;

   procedure Set_Next
      (Self : in out Nodes_Container; N, Next : Node_Access)
   is
      pragma Unreferenced (Self);
   begin
      N.Next := Next;
   end Set_Next;

   procedure Set_Previous
      (Self : in out Nodes_Container; N, Previous : Node_Access)
   is
      pragma Unreferenced (Self);
   begin
      N.Previous := Previous;
   end Set_Previous;

end Conts.Unbounded_List_Nodes;
