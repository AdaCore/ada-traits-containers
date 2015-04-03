package body Conts is

   package body Indefinite_Elements_Traits is

      -------------
      -- Release --
      -------------

      procedure Release (E : in out Element_Access) is
      begin
         Free (E.all);
         Unchecked_Free (E);
      end Release;
   end Indefinite_Elements_Traits;

end Conts;
