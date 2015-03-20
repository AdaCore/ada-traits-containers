pragma Ada_2012;

package body Conts.Lists_Impl is
   pragma Suppress (All_Checks);

   ------------
   -- Append --
   ------------

   procedure Append
      (Self    : in out List'Class;
       Element : Element_Type)
   is
      N : Node_Access := new Node'
         (Element => Convert_From (Element), Previous => null, Next => null);
   begin
      if Enable_Asserts and then N = null then
         raise Storage_Error with "Allocating node failed";
      end if;

      if Self.Tail = null then
         Self.Tail := N;
         Self.Head := Self.Tail;
      else
         Self.Tail.Next := N;
         N.Previous := Self.Tail;
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
      pragma Unreferenced (Self);
   begin
      if Enable_Asserts and then Position.Current = null then
         raise Program_Error with "Invalid position in list";
      end if;

      return Convert_To (Position.Current.Element);
   end Element;

   --------------------
   -- Stored_Element --
   --------------------

   function Stored_Element
      (Self : List'Class; Position : Cursor) return Stored_Element_Type is
      pragma Unreferenced (Self);
   begin
      if Enable_Asserts and then Position.Current = null then
         raise Program_Error with "Invalid position in list";
      end if;

      return Position.Current.Element;
   end Stored_Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : List'Class; Position : Cursor) return Boolean is
      pragma Unreferenced (Self);
   begin
      return Position.Current /= null;
   end Has_Element;

   ----------
   -- Next --
   ----------

   function Next (Self : List'Class; Position : Cursor) return Cursor is
      pragma Unreferenced (Self);
   begin
      if Position.Current = null then
         return Position;
      else
         return (Current => Position.Current.Next);
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Self : List'Class; Position : in out Cursor) is
   begin
      Position := Next (Self, Position);
   end Next;

end Conts.Lists_Impl;
