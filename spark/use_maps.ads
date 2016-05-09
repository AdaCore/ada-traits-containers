pragma Ada_2012;
with Conts; use Conts;
with Formal_Hashed_Maps;
pragma Elaborate_All (Formal_Hashed_Maps);

package Use_Maps with SPARK_Mode is

   function Hash (Id : Natural) return Hash_Type is (Hash_Type (Id));

   package My_Maps is new Formal_Hashed_Maps
     (Element_Type => Integer,
      Key_Type     => Positive,
      Hash         => Hash);

   use My_Maps;
   use type My_Maps.Cursor;
   use My_Maps.Formal_Model.P;
   use My_Maps.Formal_Model.K;
   use My_Maps.Formal_Model.M;

   function My_Contains (S : My_Maps.Map; K : Positive) return Boolean is
     (Find (S, K) /= No_Element) with
   Post => My_Contains'Result = Contains (S, K);

   --  My_Find iterates through the set to find K.

   function My_Find (S : My_Maps.Map; K : Positive) return Cursor with
     Post => My_Find'Result = Find (S, K);

   function F (E : Integer) return Integer is
      (if E in -100 .. 100 then E * 2 else E);

   --  The first two versions of Apply_F store in R the image of every element
   --  of S through F while the last two modify S in place. The difference
   --  between the two versions of each pair is that one is specified using
   --  only elements (we don't care about keys) while the other specifies
   --  that keys are preserved.

   procedure Apply_F (S : My_Maps.Map; R : in out My_Maps.Map) with
     Post => Length (R) = Length (S)
     and (for all K of S =>
              (for some L of R =>
                     Element (Model (R), L) = F (Element (Model (S), K))))
     and (for all L of R =>
              (for some K of S =>
                     Element (Model (R), L) = F (Element (Model (S), K))));

   procedure Apply_F_2 (S : My_Maps.Map; R : in out My_Maps.Map) with
     Post => Length (R) = Length (S)
     and (for all K of R => Mem (Model (S), K))
     and (for all K of S =>
              Mem (Model (R), K)
          and then Element (Model (R), K)  = F (Element (Model (S), K)));

   procedure Apply_F_3 (S : in out My_Maps.Map) with
     Post => Length (S) = Length (S)'Old
     and (for all K of Model (S)'Old =>
              (for some L of S =>
                     Element (Model (S), L) = F (Element (Model (S)'Old, K))))
     and (for all L of S =>
              (for some K of Model (S)'Old =>
                     Element (Model (S), L) = F (Element (Model (S)'Old, K))));

   procedure Apply_F_4 (S : in out My_Maps.Map) with
     Post => Length (S) = Length (S)'Old
     and Keys (S) = Keys (S)'Old
     and (for all K of S =>
              Element (Model (S), K)  = F (Element (Model (S)'Old, K)));

   --  Checks wether two maps have a disjoint set of keys.

   function Are_Disjoint (S1, S2 : My_Maps.Map) return Boolean with
     Post => Are_Disjoint'Result =
       (for all E of Model (S2) => not Mem (Model (S1), E));

   function P (E : Integer) return Boolean is
     (E >= 0);

   --  Checks that the union of two maps for which P is true only contains
   --  elements for which P is true.

   procedure Union_P (S1 : in out My_Maps.Map; S2 : My_Maps.Map) with
     Pre  => (for all K of S1 => P (Element (Model (S1), K)))
     and (for all K of S2 => P (Element (Model (S2), K)))
     and Count_Type'Last - Length (S1) > Length (S2),
     Post => (for all K of S1 => P (Element (Model (S1), K)));

   --  Test links between high-level model, lower-level position based model
   --  and lowest-level, cursor based model of a map.

   function Q (E : Integer) return Boolean;
   --  Any property Q on an Integer E.

   procedure From_Keys_To_Model (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all I in 1 .. Length (S) =>
                    Q (Element (Model (S), Get (Keys (S), I)))),
     Post   => (for all K of S => Q (Element (Model (S), K)));
   --  Test that the link can be done from a property on the elements of a
   --  low level, position based view of a container and its high level view.

   procedure From_Model_To_Keys (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all K of S => Q (Element (Model (S), K))),
     Post   => (for all I in 1 .. Length (S) =>
                    Q (Element (Model (S), Get (Keys (S), I))));
   --  Test that the link can be done from a property on the elements of a
   --  high level view of a container and its lower level, position based view.

   procedure From_Keys_To_Cursors (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all I in 1 .. Length (S) =>
                    Q (Element (Model (S), Get (Keys (S), I)))),
     Post   => (for all Cu of Positions (S) =>
                    Q (Element (Model (S), Get (Keys (S),
                  Get (Positions (S), Cu)))));
   --  Test that the link can be done from a property on the elements of a
   --  position based view of a container and its lowest level, cursor aware
   --  view.

   procedure From_Cursors_To_Keys (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all Cu of Positions (S) =>
                    Q (Element (Model (S), Get (Keys (S),
                  Get (Positions (S), Cu))))),
     Post   => (for all I in 1 .. Length (S) =>
                    Q (Element (Model (S), Get (Keys (S), I))));
   --  Test that the link can be done from a property on the elements of a
   --  cursor aware view of a container and its position based view.

   procedure From_Model_To_Cursors (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all K of S => Q (Element (Model (S), K))),
     Post   => (for all Cu of Positions (S) =>
                    Q (Element (Model (S), Get (Keys (S),
                  Get (Positions (S), Cu)))));
   --  Test that the link can be done from a property on the elements of a
   --  high level view of a container and its lowest level, cursor aware view.

   procedure From_Cursors_To_Model (S : My_Maps.Map) with
     Ghost,
     Global => null,
     Pre    => (for all Cu of Positions (S) =>
                  Q (Element (Model (S), Get (Keys (S),
                    Get (Positions (S), Cu))))),
     Post   => (for all K of S => Q (Element (Model (S), K)));
   --  Test that the link can be done from a property on the elements of a
   --  low level, cursor aware view of a container and its high level view.
end Use_Maps;
