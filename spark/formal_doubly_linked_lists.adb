pragma Ada_2012;
package body Formal_Doubly_Linked_Lists with SPARK_Mode => Off is

   function Length (Self : List'Class) return Natural is
     (Element_Lists.Lists.Length (Self));

   function Capacity (Self : List'Class) return Natural is
     (Element_Lists.Lists.Capacity (Self));

   package body Formal_Model is
      use M;
      use P;

      function Positions (Self : List'Class) return P.Map is
         Cu : Cursor := Cursor (Element_Lists.Lists.First (Self));
         R  : Map;
         I  : Positive := 1;
      begin
         while Element_Lists.Lists.Has_Element (Self, Cu) loop
            R := Add (R, Cu, I);
            Cu := Element_Lists.Lists.Next (Self, Cu);
            I := I + 1;
         end loop;
         return R;
      end Positions;

      function Model (Self : List'Class) return M.Sequence is
         Cu : Cursor := Cursor (Element_Lists.Lists.First (Self));
         R  : Sequence;
      begin
         while Element_Lists.Lists.Has_Element (Self, Cu) loop
            R := Add (R, Element_Lists.Lists.Element (Self, Cu));
            Cu := Next (Element_Lists.List (Self), Cu);
         end loop;
         return R;
      end Model;

      procedure Lift_Abstraction_Level (Self : List'Class) is null;
   end Formal_Model;

   function Element (Self : List'Class; Position : Cursor) return Element_Type
   is
      (Element_Lists.Lists.Element (Self, Position));

   function Has_Element (Self : List'Class; Position : Cursor) return Boolean
   is
      (Element_Lists.Lists.Has_Element (Self, Position));

   procedure Append (Self : in out List'Class; Element : Element_Type) is
   begin
      Element_Lists.Lists.Append (Self, Element);
   end Append;

   procedure Clear (Self : in out List'Class) is
   begin
      Element_Lists.Lists.Clear (Self);
   end Clear;

   procedure Next (Self : List'Class; Position : in out Cursor) is
   begin
      Element_Lists.Lists.Next (Self, Position);
   end Next;

   function First (Self : List'Class) return Cursor is
      (Cursor (Element_Lists.Lists.First (Self)));

end Formal_Doubly_Linked_Lists;
