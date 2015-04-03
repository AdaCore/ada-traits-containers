package body Support is

   procedure Free (Self : in out Object) is
   begin
      null;
   end Free;

   procedure Class_Wide_Free (Self : in out Object'Class) is
   begin
      Free (Self);  --  dispatching
   end Class_Wide_Free;

   overriding procedure Free (Self : in out Object2) is
   begin
      null;
   end Free;

end Support;
