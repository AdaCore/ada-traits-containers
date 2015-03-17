pragma Ada_2012;

package body Conts.Lists is
   pragma Suppress (All_Checks);

   ------------
   -- Append --
   ------------

   procedure Append
      (Self    : in out List;
       Element : Element_Type)
   is
      N : Node_Access := new Node'
         (Element => Element, Previous => null, Next => null);
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
   end Append;

   -----------
   -- First --
   -----------

   function First (Self : List) return Cursor is
   begin
      return (Current => Self.Head);
   end First;

   -------------
   -- Element --
   -------------

   function Element (Self : List; Position : Cursor) return Element_Type is
      pragma Unreferenced (Self);
   begin
      if Enable_Asserts and then Position.Current = null then
         raise Program_Error with "Invalid position in list";
      end if;

      return Position.Current.Element;
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Self : List; Position : Cursor) return Boolean is
      pragma Unreferenced (Self);
   begin
      return Position.Current /= null;
   end Has_Element;

   ----------
   -- Next --
   ----------

   function Next (Self : List; Position : Cursor) return Cursor is
      pragma Unreferenced (Self);
   begin
      if Position.Current = null then
         return Position;
      else
         return (Current => Position.Current.Next);
      end if;
   end Next;

end Conts.Lists;
