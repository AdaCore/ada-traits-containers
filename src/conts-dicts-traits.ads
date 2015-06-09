pragma Ada_2012;
with Conts.Dicts.Strings; use Conts.Dicts.Strings;

package Conts.Dicts.Traits is

   package String_Traits is new Elements_Traits
     (Element_Type        => String,
      Stored_Element_Type => Stored_String,
      Reference_Type      => String, -- ??? how to implement this
      Convert_From        => Convert_From,
      Convert_To          => Convert_To,
      Get_Reference       => Get_Reference,
      Release             => Release,
      Use_Implicit_Copy   => False);

   generic
      type Value_Type is private;
      with procedure Free (E : in out Value_Type) is null;

   package Definite_String_Dict_Traits is

      package Value_Traits is new Definite_Elements_Traits (Value_Type, Free);

      package Traits is new Conts.Dicts.Dict_Traits
        (Unsigned_32,
         String_Traits,
         Value_Traits.Elements,
         Hash => Hash,
         Equals => "=",
         Container_Size => Dynamic_Container_Size);
   end Definite_String_Dict_Traits;

   generic
      type Value_Type is private;
      with procedure Free (E : in out Value_Type) is null;

   package Indefinite_String_Dict_Traits is

      package Value_Traits is new Indefinite_Elements_Traits (Value_Type, Free);

      package Traits is new Conts.Dicts.Dict_Traits
        (Unsigned_32,
         String_Traits,
         Value_Traits.Elements,
         Hash => Hash,
         Equals => "=",
         Container_Size => Dynamic_Container_Size);

   end Indefinite_String_Dict_Traits;


end Conts.Dicts.Traits;
