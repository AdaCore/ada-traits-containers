with Refcount;            use Refcount;
with Refcount_Ref;        use Refcount_Ref;
with Refcount_Traits;     use Refcount_Traits;
with GNATCOLL.Refcount;   use GNATCOLL.Refcount;
with Conts;               use Conts;

package Support is

   --------------
   -- Refcount --
   --------------

   type Object is tagged record
      null;
   end record;
   procedure Free (Self :  in out Object);
   procedure Class_Wide_Free (Self : in out Object'Class)
      with Inline => True;

   type Child is new Object with null record;

   package Int_Pointers_Unsafe is new Refcount.Smart_Pointers
      (Integer, Thread_Safe => False);
   package Int_Pointers is new Refcount.Smart_Pointers (Integer);
   package Obj_Pointers is new Refcount.Smart_Pointers (Object'Class);
   package Obj_Pointers_Free is new Refcount.Smart_Pointers
      (Object'Class, Free => Class_Wide_Free);

   --------------------------
   -- Refcount with traits --
   --------------------------

   package Int_Elements is new Definite_Elements_Traits (Integer);
   package Obj_Elements_No_Free is new Indefinite_Elements_Traits
      (Object'Class);
   package Obj_Elements is new Indefinite_Elements_Traits
      (Object'Class, Class_Wide_Free);

   package Int_Pointers_Traits_Unsafe is new Refcount_Traits.Smart_Pointers
      (Int_Elements.Elements, Thread_Safe => False);
   package Int_Pointers_Traits is new Refcount_Traits.Smart_Pointers
      (Int_Elements.Elements);
   package Obj_Pointers_Traits is new Refcount_Traits.Smart_Pointers
      (Obj_Elements_No_Free.Elements);
   package Obj_Pointers_Free_Traits is new Refcount_Traits.Smart_Pointers
      (Obj_Elements.Elements);

   --------------------------------
   -- Refcount as reference_type --
   --------------------------------

   package Int_Pointers_Ref is new Refcount_Ref.Smart_Pointers
      (Integer);
   package Obj_Pointers_Ref is new Refcount_Ref.Smart_Pointers
      (Object'Class);
   package Obj_Pointers_Free_Ref is new Refcount_Ref.Smart_Pointers
      (Object'Class, Class_Wide_Free);

   -----------------------
   -- GNATCOLL Refcount --
   -----------------------

   type Integer_Object is new Refcounted with record
      Value : Integer;
   end record;

   type Object2 is new Refcounted with record
      null;
   end record;
   overriding procedure Free (Self :  in out Object2);

   type Child2 is new Object2 with null record;

   package Int_Pointers_Gnatcoll is new GNATCOLL.Refcount.Smart_Pointers
      (Integer_Object);
   package Obj_Pointers_Gnatcoll is new GNATCOLL.Refcount.Smart_Pointers
      (Object2);

end Support;
