with Refcount;            use Refcount;
with Refcount_Ref;        use Refcount_Ref;
with GNATCOLL.Refcount;   use GNATCOLL.Refcount;
with GNAT.Strings;        use GNAT.Strings;

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
      (Integer, Atomic_Counters => False);
   package Int_Pointers is new Refcount.Smart_Pointers (Integer);
   package Obj_Pointers is new Refcount.Smart_Pointers (Object'Class);
   package Obj_Pointers_Free is new Refcount.Smart_Pointers
      (Object'Class, Release => Class_Wide_Free);
   package String_Pointers is new Refcount.Smart_Pointers (String);

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

   type String_Object is new Refcounted with record
      Str : GNAT.Strings.String_Access;
   end record;
   procedure Free (Str : in out String_Object);

   type Object2 is new Refcounted with record
      null;
   end record;
   overriding procedure Free (Self :  in out Object2);

   type Child2 is new Object2 with null record;

   package Int_Pointers_Gnatcoll is new GNATCOLL.Refcount.Smart_Pointers
      (Integer_Object);
   package Obj_Pointers_Gnatcoll is new GNATCOLL.Refcount.Smart_Pointers
      (Object2);
   package String_Pointers_Gnatcoll is new GNATCOLL.Refcount.Smart_Pointers
      (String_Object);

end Support;
