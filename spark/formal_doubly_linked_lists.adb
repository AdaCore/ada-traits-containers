pragma Ada_2012;
package body Formal_Doubly_Linked_Lists with SPARK_Mode => Off is

   function Length (L : List'Class) return Natural is
     (Element_Lists.Lists.Length (L));

   function Capacity (L : List'Class) return Natural is
     (Element_Lists.Lists.Capacity (L));

   package body Formal_Model is
      function Positions (L : List'Class) return Map is
         Cu : Cursor := Cursor (First (Element_Lists.List (L)));
         R  : Map := Cursor_Map.Empty;
         I  : Positive := 1;
      begin
         while Has_Element (Element_Lists.List (L), Cu) loop
            R := Add (R, Cu, I);
            Next (Element_Lists.List (L), Cu);
            I := I + 1;
         end loop;
         return R;
      end Positions;
      function Model (L : List'Class) return Sequence is
         Cu : Cursor := Cursor (First (Element_Lists.List (L)));
         R  : Sequence := Element_Sequence.Empty;
      begin
         while Has_Element (Element_Lists.List (L), Cu) loop
            R := Add (R, Element (Element_Lists.List (L), Cu));
            Next (Element_Lists.List (L), Cu);
         end loop;
         return R;
      end Model;
   end Formal_Model;

   function Element (L : List'Class; C : Cursor) return Element_Type is
      (Element_Lists.Lists.Element (L, Element_Lists.Lists.Cursor (C)));

   function Has_Element (L : List'Class; C : Cursor) return Boolean is
      (Element_Lists.Lists.Has_Element (L, Element_Lists.Lists.Cursor (C)));

   procedure Append (L : in out List'Class; E : Element_Type) is
   begin
      Element_Lists.Lists.Append (L, E);
   end Append;

   procedure Clear (L : in out List'Class) is
   begin
      Element_Lists.Lists.Clear (L);
   end Clear;

   procedure Next (L : List'Class; C : in out Cursor) is
   begin
      Element_Lists.Lists.Next (L, Element_Lists.Lists.Cursor (C));
   end Next;

   function First (L : List'Class) return Cursor is
      (Cursor (Element_Lists.Lists.First (L)));

end Formal_Doubly_Linked_Lists;
