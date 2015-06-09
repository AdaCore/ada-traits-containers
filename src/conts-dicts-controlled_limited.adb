package body Conts.Dicts.Controlled_Limited is

   package body Generic_Limited_Dicts is

      procedure Allocate (Self : in out Dict; Min_Size : Index_Type := 0) is
      begin
         Dicts.Allocate (Self.D);
      end Allocate;

      function Element (Self : Dict; Key : Key_Type) return Cursor is
      begin
         return Dicts.Element (Self.D, Key);
      end Element;

      function Has_Element (Self : Dict; Position : Cursor) return Boolean is
      begin
         return Dicts.Has_Element (Self.D, Position);
      end Has_Element;

      procedure Release (Self : in out Dict) is
      begin
         Dicts.Release (Self.D);
      end Release;

      procedure Set (Self     : in out Dict;
                     Position : Cursor;
                     Key      : Key_Type;
                     Value    : Value_Type;
                     Enable_Realloc : Boolean := True) is
      begin
         Dicts.Set (Self.D, Position, Key, Value, Enable_Realloc);
      end Set;

      function Value (Self : Dict; Position : Cursor) return Value_Type is
      begin
         return Dicts.Value (Self.D, Position);
      end Value;

   end Generic_Limited_Dicts;

end Conts.Dicts.Controlled_Limited;
