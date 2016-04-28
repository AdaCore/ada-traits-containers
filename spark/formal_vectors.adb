package body Formal_Vectors with SPARK_Mode => Off is

   function Length (V : Vector'Class) return Natural is
     (Element_Vectors.Vectors.Length (V));

   function To_Index (Position : Cursor) return Index_Type is
     (Element_Vectors.Vectors.To_Index (Position));

   package body Formal_Model is
      function Valid_Cursors (V : Vector'Class) return Set is
         Cu : Cursor := Cursor (First (Element_Vectors.Vector (V)));
         R  : Set;
      begin
         while Element_Vectors.Vectors.Has_Element (V, Cu) loop
            R := Add (R, Cu);
            Cu := Element_Vectors.Vectors.Next (V, Cu);
         end loop;
         return R;
      end Valid_Cursors;

      function Model (V : Vector'Class) return Sequence is
         R  : Sequence;
      begin
         if Element_Vectors.Vectors.Is_Empty (V) then
            return R;
         end if;

         for I in Index_Type'First .. Element_Vectors.Vectors.Last (V) loop
            R := Add (R, Element_Vectors.Vectors.Element (V, I));
         end loop;
         return R;
      end Model;
   end Formal_Model;

   procedure Reserve_Capacity
     (Self : in out Vector'Class; Capacity : Count_Type) is
   begin
      Element_Vectors.Vectors.Reserve_Capacity (Self, Capacity);
   end Reserve_Capacity;

   procedure Shrink_To_Fit (Self : in out Vector'Class) is
   begin
      Element_Vectors.Vectors.Shrink_To_Fit (Self);
   end Shrink_To_Fit;

   procedure Resize
     (Self    : in out Vector'Class;
      Length  : Index_Type;
      Element : Element_Type)
   is
   begin
      Element_Vectors.Vectors.Resize (Self, Length, Element);
   end Resize;

   function Is_Empty (Self : Vector'Class) return Boolean is
     (Element_Vectors.Vectors.Is_Empty (Self));

   function Last (Self : Vector'Class) return Extended_Index is
     (if Element_Vectors.Vectors.Is_Empty (Self) then Extended_Index'First
      else Element_Vectors.Vectors.Last (Self));

   procedure Append
     (Self    : in out Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
   is
   begin
      Element_Vectors.Vectors.Append (Self, Element, Count);
   end Append;

   procedure Replace_Element
     (Self     : in out Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
   is
   begin
      Element_Vectors.Vectors.Replace_Element (Self, Index, New_Item);
   end Replace_Element;

   procedure Swap
     (Self        : in out Vector'Class;
      Left, Right : Index_Type)
   is
   begin
      Element_Vectors.Vectors.Swap (Self, Left, Right);
   end Swap;

   procedure Clear (Self : in out Vector'Class) is
   begin
      Element_Vectors.Vectors.Clear (Self);
   end Clear;

   procedure Delete (Self : in out Vector'Class; Index : Index_Type)
   is
   begin
      Element_Vectors.Vectors.Delete (Self, Index);
   end Delete;

   procedure Delete_Last (Self : in out Vector'Class) is
   begin
      Element_Vectors.Vectors.Delete_Last (Self);
   end Delete_Last;

   function Last_Element (Self : Vector'Class) return Element_Type is
     (Element_Vectors.Vectors.Last_Element (Self));

   procedure Assign
     (Self : in out Vector'Class; Source : Vector'Class)
   is
   begin
      Element_Vectors.Vectors.Assign (Self, Source);
   end Assign;

   function Element
     (Self : Vector'Class; Position : Index_Type) return Element_Type
   is
     (Element_Vectors.Vectors.Element (Self, Position));

   function First (Self : Vector'Class) return Cursor is
     (Element_Vectors.Vectors.First (Self));

   function Element
     (Self : Vector'Class; Position : Cursor) return Element_Type
   is
     (Element_Vectors.Vectors.Element (Self, Position));

   function Has_Element
     (Self : Vector'Class; Position : Cursor) return Boolean
   is
      (Element_Vectors.Vectors.Has_Element (Self, Position));

   function Next
     (Self : Vector'Class; Position : Cursor) return Cursor
   is
     (Element_Vectors.Vectors.Next (Self, Position));

   function Previous
     (Self : Vector'Class; Position : Cursor) return Cursor
   is
     (Element_Vectors.Vectors.Previous (Self, Position));

   procedure Next (Self : Vector'Class; Position : in out Cursor) is
   begin
      Element_Vectors.Vectors.Next (Self, Position);
   end Next;

end Formal_Vectors;
