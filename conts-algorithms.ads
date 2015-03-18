package Conts.Algorithms is

   generic
      with package Cursors is new Forward_Cursors_Traits (<>);
   function Count_If
      (Self      : Cursors.Container;
       Predicate : access function (E : Cursors.Element_Type) return Boolean)
      return Natural;
   --  Should we have a version that takes a 'From:Cursor' parameter ?

end Conts.Algorithms;
