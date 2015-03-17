with Ada.Containers.Doubly_Linked_Lists;
with Mylists;

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

   generic
      type T is private;
      type Cursor is private;
      with function Element (C : Cursor) return T is <>;
      with function Has_Element (C : Cursor) return Boolean is <>;
      with procedure Next (C : in out Cursor) is <>;
   package Forward_Iterators is
      --  ??? Should have a nested package for Input_Iterators
      --  ??? Same for Random_Iterators, which should provide a version for
      --      Forward_Iterators.
      --  ??? Should define equality if we do not use Has_Element

      --  Define the types so that we can access the generic formals from an
      --  instance of this package.
      subtype Element_Type is T;
      subtype Cursor_Type is Cursor;
   end Forward_Iterators;

   generic
      with package Cursors is new Forward_Iterators (others => <>);
      with package Predicate is new Unary_Functors
         (Param => Cursors.Element_Type, Ret => Boolean);
   function Generic_Count
      (Start : Cursors.Cursor; F : Predicate.Obj'Class) return Natural;
   --  Count the number of elements in the sequence for which
   --  F returns True.

   --------------
   -- Examples --
   --------------

   package Integer_Lists is new Ada.Containers.Doubly_Linked_Lists (Integer);
   package List_Iterators is new Forward_Iterators
      (T           => Integer, --  ??? can't use Integer_Lists.Element_Type
       Cursor      => Integer_Lists.Cursor,
       Element     => Integer_Lists.Element,
       Has_Element => Integer_Lists.Has_Element,
       Next        => Integer_Lists.Next);
   package Unary_Predicates is new Unary_Functors
      (Param => Integer, Ret => Boolean);

   function Greater_Than_3 (P : Integer) return Boolean is (P > 3)
      with Inline => True;
   function Greater_Than (P, Q : Integer) return Boolean is (P > Q)
      with Inline => True;

   --------------------------------
   -- Using access to subprogram --
   --------------------------------

   function Count
      (L : Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural;
   --  ??? Seems slower with  "with Inline=>True"

   function Count_With_Equal
      (L : Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural
      with Inline => True;
   --  Using "=" on Cursor, instead of Has_Element

   function Count_With_Iterator
      (L : Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural;

   generic
      with function Predicate (P : Integer) return Boolean;
   function Count_With_Generic_Func
      (L : Mylists.My_Integer_Lists.List) return Natural;

   function Count_Separate_Pkg
      (L : Mylists.My_Integer_Lists.List;
       Predicate : access function (P : Integer) return Boolean)
      return Natural
      with Inline => True;
   --  Same as count, but with a containers package instantiated in another
   --  package.

   generic
      with package Predicate is new Unary_Functors
         (Param => Integer, Ret => Boolean);
   function Count_With_Functor
      (L : Integer_Lists.List; F : Predicate.Obj'Class) return Natural;

   ---------------------------
   -- Using virtual methods --
   ---------------------------
   --  This simulates a hierarchy of containers and iterators, as opposed to
   --  using generics.

   generic
      type T is private;
   package Containers_Hierarchy is
      type Forward_Cursor is interface;
      function Element (C : Forward_Cursor) return T is abstract;
      procedure Next (C : in out Forward_Cursor) is abstract;
      function Has_Element (C : Forward_Cursor) return Boolean is abstract;

      type Container is interface;
      function First (C : Container) return Forward_Cursor'Class is abstract;
      procedure Append (C : in out Container; P : T) is abstract;

      package Internal_Lists is new Ada.Containers.Doubly_Linked_Lists (T);

      type List is new Container with record
         L : Internal_Lists.List;
      end record;
      overriding function First (C : List) return Forward_Cursor'Class;
      overriding procedure Append (C : in out List; P : T);

      type List_Cursor is new Forward_Cursor with record
         C : Internal_Lists.Cursor;
      end record;
      overriding function Element (C : List_Cursor) return T;
      overriding procedure Next (C : in out List_Cursor);
      overriding function Has_Element (C : List_Cursor) return Boolean;
   end Containers_Hierarchy;

   generic
      with package Containers is new Containers_Hierarchy (<>);
      with function Predicate (P : Containers.T) return Boolean;
   function Generic_Count_Virtual
      (C : Containers.Forward_Cursor'Class) return Natural;


end Perf_Support;
