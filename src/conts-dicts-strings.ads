package Conts.Dicts.Strings is

   type Stored_String is private;

   function Convert_From (E : String) return Stored_String;

   function Convert_To (E : Stored_String) return String;

   function Get_Reference (E : Stored_String) return String;
   -- how to implement this in this context

   procedure Release (E : in out Stored_String);

   function Hash (Str : String) return Unsigned_32;

private

   type String_Access is access String;
   type String_Storage_Kind is (Short_String, Long_String);
   Short_String_Size : constant := 12;

   type Stored_String (Kind : String_Storage_Kind := Short_String) is record
      case Kind is
         when Short_String =>
            Short_Value  : String (1 .. Short_String_Size);
            Short_Length : Integer;
         when Long_String =>
            Long_Value   : String_Access := null;
      end case;
   end record;
end Conts.Dicts.Strings;
