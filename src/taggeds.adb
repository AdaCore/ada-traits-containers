package body Taggeds is
   overriding function First (C : List) return Forward_Cursor'Class is
   begin
      return List_Cursor'(C => C.L.First);
   end First;

   overriding procedure Append (C : in out List; P : T) is
   begin
      Internal_Lists.Append (C.L, P);
   end Append;

   overriding function Element (C : List_Cursor) return T is
   begin
      return Internal_Lists.Element (C.C);
   end Element;

   overriding procedure Next (C : in out List_Cursor) is
   begin
      Internal_Lists.Next (C.C);
   end Next;

   overriding function Has_Element (C : List_Cursor) return Boolean is
   begin
      return Internal_Lists.Has_Element (C.C);
   end Has_Element;
end Taggeds;
