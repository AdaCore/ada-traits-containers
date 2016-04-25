pragma Ada_2012;
package body Formal_Doubly_Linked_Lists with SPARK_Mode => Off is

   function Length (L : List'Class) return Natural is
     (Element_Lists.Lists.Length (L));

   function Capacity (L : List'Class) return Natural is
     (Element_Lists.Lists.Capacity (L));

   package body Formal_Model is
      function Positions (L : List'Class) return Map is
         Cu : Cursor := Cursor (Element_Lists.Lists.First (L));
         R  : Map;
         I  : Positive := 1;
      begin
         while Element_Lists.Lists.Has_Element (L, Cu) loop
            R := Add (R, Cu, I);
            Cu := Element_Lists.Lists.Next (L, Cu);
            I := I + 1;
         end loop;
         return R;
      end Positions;

      function Model (L : List'Class) return Sequence is
         Cu : Cursor := Cursor (Element_Lists.Lists.First (L));
         R  : Sequence;
      begin
         while Element_Lists.Lists.Has_Element (L, Cu) loop
            R := Add (R, Element_Lists.Lists.Element (L, Cu));
            Cu := Next (Element_Lists.List (L), Cu);
         end loop;
         return R;
      end Model;
   end Formal_Model;

   function Element (L : List'Class; C : Cursor) return Element_Type is
      (Element_Lists.Lists.Element (L, C));

   function Has_Element (L : List'Class; C : Cursor) return Boolean is
      (Element_Lists.Lists.Has_Element (L, C));

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
      Element_Lists.Lists.Next (L, C);
   end Next;

   function First (L : List'Class) return Cursor is
      (Cursor (Element_Lists.Lists.First (L)));

end Formal_Doubly_Linked_Lists;
