pragma Ada_2012;
with Conts; use Conts;
with Conts.Maps.Indef_Indef_Unbounded_SPARK;
with Conts.Algorithms.SPARK;
pragma Elaborate_All (Conts.Maps.Indef_Indef_Unbounded_SPARK);

package Use_Maps with SPARK_Mode is

   function Hash (Id : Natural) return Hash_Type is (Hash_Type (Id));

   package My_Maps is new Conts.Maps.Indef_Indef_Unbounded_SPARK
     (Element_Type => Integer,
      Key_Type     => Positive,
      Hash         => Hash);

   use type My_Maps.Cursor;
   use My_Maps.Impl;
   use all type My_Maps.Model_Map;
   use all type My_Maps.Key_Sequence;
   use all type My_Maps.Cursor_Position_Map;

   pragma Unevaluated_Use_Of_Old (Allow);

   function Find is new Conts.Algorithms.SPARK.Find
     (Cursors => My_Maps.Cursors.Forward,
      Getters => My_Maps.Maps.Constant_Returned_Key,
      "="     => "=",
      Content => My_Maps.Content_Models);

   function My_Find (S : My_Maps.Map; K : Positive) return Cursor
     with
       Post => (if My_Find'Result /= No_Element
                then Has_Element (S, My_Find'Result)
                  and then As_Key (S, My_Find'Result) = K
                else not Mem (Model (S), K));
   --  Iterate through the set to find K.

   function My_Contains (S : My_Maps.Map; K : Positive) return Boolean
     is (Impl.Contains (S, K)) with
   Post => (if My_Contains'Result then My_Find (S, K) /= No_Element
            else My_Find (S, K) = No_Element);

   function F (E : Integer) return Integer is
      (if E in -100 .. 100 then E * 2 else E);

   procedure Apply_F (S : My_Maps.Map; R : in out My_Maps.Map) with
     Post => Length (R) = Length (S)
     and (for all K of S =>
              (for some L of R => As_Element (R, L) = F (As_Element (S, K))))
     and (for all L of R =>
              (for some K of S => As_Element (R, L) = F (As_Element (S, K))));
   --  Store in R the image of every element of S through F.

   procedure Apply_F_2 (S : My_Maps.Map; R : in out My_Maps.Map) with
     Post => Length (R) = Length (S)
     and (for all K of R => Mem (Impl.Model (S), K))
     and (for all K of S => Mem (Impl.Model (R), K)
          and then As_Element (R, K)  = F (As_Element (S, K)));
   --  Same as before except that we also want S_Keys to be preserved.

   procedure Apply_F_3 (S : in out My_Maps.Map) with
     Post => Length (S) = Length (S)'Old
     and (for all K of Impl.Model (S)'Old =>
              (for some L of S =>
                     As_Element (S, L) = F (Element (Impl.Model (S)'Old, K))))
     and (for all L of S =>
              (for some K of Impl.Model (S)'Old =>
                     As_Element (S, L) = F (Element (Impl.Model (S)'Old, K))));
   --  Replace every element of S by its image through F.

   procedure Apply_F_4 (S : in out My_Maps.Map) with
     Post => Length (S) = Length (S)'Old
     and S_Keys (S) = S_Keys (S)'Old
     and (for all K of S =>
              As_Element (S, K)  = F (Element (Impl.Model (S)'Old, K)));
   --  Same as before except that we also want S_Keys to be preserved.

   function Are_Disjoint (S1, S2 : My_Maps.Map) return Boolean with
     Post => Are_Disjoint'Result =
       (for all E of S2 => not Mem (Impl.Model (S1), E));
   --  Check wether two maps have a disjoint set of S_Keys.

   function P (E : Integer) return Boolean is
     (E >= 0);

   procedure Union_P (S1 : in out My_Maps.Map; S2 : My_Maps.Map) with
     Pre  => (for all K of S1 => P (As_Element (S1, K)))
     and (for all K of S2 => P (As_Element (S2, K)))
     and Count_Type'Last - Length (S1) > Length (S2),
     Post => (for all K of S1 => P (As_Element (S1, K)));
   --  Compute the union of two maps for which P is true.

   Count : constant := 5;

   procedure Insert_Count (M : in out My_Maps.Map)
   --  Insert 0 Count times at S_Keys 1 .. Count.

   with
     Pre  => Count_Type'Last - Count > Length (M),
     Post => Length (M) <= Length (M)'Old + Count
     and (for all K of M =>
            (if K > Count then Mem (Impl.Model (M)'Old, K)
             and then As_Element (M, K) =
                 Impl.M.Get (Impl.Model (M)'Old, K)))
     and (for all K in 1 .. Count => Mem (Impl.Model (M), K)
          and then Get (M, K) = 0);

   --  Test links between high-level Model, lower-level position based Model
   --  and lowest-level, cursor based Impl.Model of a map.

   function Q (E : Integer) return Boolean;
   --  Any property Q on an Integer E.

   procedure From_S_Keys_To_Model (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all I in 1 .. Length (S) =>
                    Q (As_Element (S, Get (S_Keys (S), I)))),
     Post   => (for all K of S => Q (As_Element (S, K)));
   --  Test that the link can be done from a property on the elements of a
   --  low level, position based view of a container and its high level view.

   procedure From_Model_To_S_Keys (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all K of S => Q (As_Element (S, K))),
     Post   => (for all I in 1 .. Length (S) =>
                    Q (As_Element (S, Get (S_Keys (S), I))));
   --  Test that the link can be done from a property on the elements of a
   --  high level view of a container and its lower level, position based view.

   procedure From_S_Keys_To_Cursors (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all I in 1 .. Length (S) =>
                    Q (As_Element (S, Get (S_Keys (S), I)))),
     Post   => (for all Cu in S => Q (Element (S, Cu)));
   --  Test that the link can be done from a property on the elements of a
   --  position based view of a container and its lowest level, cursor aware
   --  view.

   procedure From_Cursors_To_S_Keys (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all Cu in S => Q (Element (S, Cu))),
     Post   => (for all I in 1 .. Length (S) =>
                    Q (As_Element (S, Get (S_Keys (S), I))));
   --  Test that the link can be done from a property on the elements of a
   --  cursor aware view of a container and its position based view.

   procedure From_Model_To_Cursors (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all K of S => Q (As_Element (S, K))),
     Post   => (for all Cu in S => Q (Element (S, Cu)));
   --  Test that the link can be done from a property on the elements of a
   --  high level view of a container and its lowest level, cursor aware view.

   procedure From_Cursors_To_Model (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all Cu in S => Q (Element (S, Cu))),
     Post   => (for all K of S => Q (As_Element (S, K)));
   --  Test that the link can be done from a property on the elements of a
   --  low level, cursor aware view of a container and its high level view.
end Use_Maps;
