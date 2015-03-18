with Ada.Containers.Doubly_Linked_Lists;

package Perf_Support is

   --  Performance testing when using generics, virtual methods and
   --  access to subprograms.

   --------------------
   -- Unary functors --
   --------------------
   --  These packages provide objects that act like functions, except it is
   --  possible to store data in them. They can thus be used to write
   --  accumulators, or to curry binary functions into unary functions.

   generic
      type Param is private;
      type Ret is private;
   package Unary_Functors is
      --  Define the types so that we can access the generic formals from an
      --  instance of this package.
      subtype Param_Type is Param;
      subtype Return_Type is Ret;

      type Obj is interface;
      function Call (Self : Obj; P : Param) return Ret is abstract;
   end Unary_Functors;

   generic
      with package F is new Unary_Functors (others => <>);
      with function Call (P : F.Param_Type) return F.Return_Type;
   package Unary_Functions is
      package Functors renames F;

      type Obj is new F.Obj with null record;
      overriding function Call
         (Self : Obj; P : F.Param_Type) return F.Return_Type
         is (Call (P));

      Make : constant Obj := (null record);
   end Unary_Functions;

   --------------------
   -- Using generics --
   --------------------

   generic
      type Param1 is private;
      type Param2 is private;
      type Ret is private;
      with function Call (P : Param1; Q : Param2) return Ret;
   package Binary_Functions is
      --  Define the types so that we can access the generic formals from an
      --  instance of this package.
      subtype Param1_Type is Param1;
      subtype Param2_Type is Param2;
      subtype Return_Type is Ret;
   end Binary_Functions;

   generic
      type Param1 is private;
      type Param2 is private;
      B : Param2;
      type Ret is private;
      with function Call (P : Param1; Q : Param2) return Ret;
   package Binder2nd is
      --  Define the types so that we can access the generic formals from an
      --  instance of this package.
      subtype Param1_Type is Param1;
      subtype Param2_Type is Param2;
      subtype Return_Type is Ret;

      function Binder_Call (P : Param1_Type) return Return_Type
         is (Call (P, B));
      --  package Funcs is new Unary_Functors
      --     (Param => Param1_Type,
      --      Ret   => Return_Type,
      --      Call  => Binder_Call);
   end Binder2nd;
   --  ??? Not the same as in C++ STL: since we do not have a class, there is no
   --  constructor to pass the value of the constant, which is therefore part of the
   --  generic formals, so we need a different instantiation for every possible value
   --  we might want for B.

   -----------
   -- Tests --
   -----------

   Repeat      : constant := 10;
   Items_Count : constant := 10_000_000;

   procedure Test_Conts_Lists;
   --  Perform the tests for our own Conts containers

   procedure Test_Cpp;
   pragma Import (C, Test_Cpp, "test_c");
   --  Perform C++ testing

   procedure Test_Ada2012;
   --  Test Ada2012 containers

   procedure Test_Tagged;
   --  Test when the list is implemented as tagged types

end Perf_Support;
