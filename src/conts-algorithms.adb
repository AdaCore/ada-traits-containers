package body Conts.Algorithms is

   --------------
   -- Count_If --
   --------------

   function Count_If
      (Self      : Cursors.Container;
       Predicate : access function (E : Cursors.Element_Type) return Boolean)
      return Natural
   is
      C : Cursors.Cursor := Cursors.First (Self);
      Count : Natural := 0;
   begin
      while Cursors.Has_Element (Self, C) loop
         if Predicate (Cursors.Element (Self, C)) then
            Count := Count + 1;
         end if;
         C := Cursors.Next (Self, C);
      end loop;
      return Count;
   end Count_If;

end Conts.Algorithms;
