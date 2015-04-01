package body Conts.Lists.Definite_Bounded_Limited is

   ----------
   -- Copy --
   ----------

   function Copy (Self : List'Class) return List'Class is
   begin
      return Result : List (Capacity => Self.Capacity) do
         Result.Assign (Self);
      end return;
   end Copy;

end Conts.Lists.Definite_Bounded_Limited;
