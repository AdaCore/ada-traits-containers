pragma Ada_2012;

package body Conts.Lists_Impl is

   use All_Nodes;

   ------------
   -- Append --
   ------------

   procedure Append
      (Self    : in out List'Class;
       Element : Element_Type)
   is
      N : Node_Access;
   begin
      Allocate
         (Self.Nodes,
          All_Nodes.Elements.Convert_From (Element),
          New_Node => N);

      if Enable_Asserts and then N = Null_Access then
         raise Storage_Error with "Allocating node failed";
      end if;

      if Self.Tail = Null_Access then
         Self.Tail := N;
         Self.Head := Self.Tail;
      else
         Set_Next (Self.Nodes, Self.Tail, Next => N);
         Set_Previous (Self.Nodes, N, Previous => Self.Tail);
         Self.Tail := N;
      end if;

      Self.Size := Self.Size + 1;
   end Append;

   ------------
   -- Length --
   ------------

   function Length (Self : List'Class) return Count_Type is
   begin
      return Self.Size;
   end Length;

   --------------
   -- Capacity --
   --------------

   function Capacity (Self : List'Class) return Count_Type is
      pragma Unreferenced (Self);
   begin
      return Count_Type'Last;
   end Capacity;

   -----------
   -- First --
   -----------

   function First (Self : List'Class) return Cursor is
   begin
      return (Current => Self.Head);
   end First;

   -------------
   -- Element --
   -------------

   function Element (Self : List'Class; Position : Cursor) return Element_Type is
   begin
      if Enable_Asserts and then Position.Current = Null_Access then
         raise Program_Error with "Invalid position in list";
      end if;

      return All_Nodes.Elements.Convert_To
         (Get_Element (Self.Nodes, Position.Current));
   end Element;

   --------------------
   -- Stored_Element --
   --------------------

   function Stored_Element
      (Self : List'Class; Position : Cursor) return Stored_Element_Type is
   begin
      if Enable_Asserts and then Position.Current = Null_Access then
         raise Program_Error with "Invalid position in list";
      end if;

      return Get_Element (Self.Nodes, Position.Current);
   end Stored_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : List'Class; Position : Cursor) return Boolean is
      pragma Unreferenced (Self);
   begin
      return Position.Current /= Null_Access;
   end Has_Element;

   ----------
   -- Next --
   ----------

   function Next (Self : List'Class; Position : Cursor) return Cursor is
   begin
      if Position.Current = Null_Access then
         return Position;
      else
         return (Current => Get_Next (Self.Nodes, Position.Current));
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Self : List'Class; Position : Cursor) return Cursor is
   begin
      if Position.Current = Null_Access then
         return Position;
      else
         return (Current => Get_Previous (Self.Nodes, Position.Current));
      end if;
   end Previous;

   ----------
   -- Next --
   ----------

   procedure Next (Self : List'Class; Position : in out Cursor) is
   begin
      Position := Next (Self, Position);
   end Next;

end Conts.Lists_Impl;
