pragma Ada_2012;
with Formal_Hashed_Sets;
pragma Elaborate_All (Formal_Hashed_Sets);

package Use_Sets with SPARK_Mode is
   package My_Sets is new Formal_Hashed_Sets
     (Element_Type => Integer,
      None         => Integer'First);
   use My_Sets;
   use type My_Sets.Cursor;
   use My_Sets.Formal_Model;
   use My_Sets.Formal_Model.Cursor_Map;
   use My_Sets.Formal_Model.Element_Sequence;
   use My_Sets.Formal_Model.Element_Set;

   function My_Contains (S : My_Sets.Set; E : Integer) return Boolean is
     (Find (S, E) /= No_Element) with
   Post => My_Contains'Result = Contains (S, E);

   --  My_Find iterates through the set to find E.

   function My_Find (S : My_Sets.Set; E : Integer) return Cursor with
     Post => My_Find'Result = Find (S, E);

   --  Apply_F stores in R the image of every element of S through F. F is not
   --  injective.

   function F (E : Integer) return Integer is
      (if E in -100 .. 100 then E * 2 else E);

   procedure Apply_F (S : My_Sets.Set; R : in out My_Sets.Set) with
     Pre  => Capacity (R) >= Length (S),
     Post => Capacity (R) = Capacity (R)'Old
     and (for all E in Model (S) => Mem (Model (R), F (E)))
     and (for all G in Model (R) =>
              (for some E in Model (S) => G = F (E)));

   --  Checks wether two sets are disjoint. Specify it using either the
   --  intersection or a quantified expression.

   function Are_Disjoint (S1, S2 : My_Sets.Set) return Boolean with
     Post => Are_Disjoint'Result =
       Is_Empty (Intersection (Model (S1), Model (S2)));

   function Are_Disjoint_2 (S1, S2 : My_Sets.Set) return Boolean with
     Post => Are_Disjoint_2'Result =
       (for all E in Model (S2) => not Mem (Model (S1), E));

   --  Checks that the union of two sets for which P is true only contains
   --  elements for which P is true.

   function P (E : Integer) return Boolean is
     (E >= 0);

   procedure Union_P (S1 : in out My_Sets.Set; S2 : My_Sets.Set) with
     Pre  => (for all E in Model (S1) => P (E))
     and (for all E in Model (S2) => P (E))
     and Capacity (S1) - Length (S1) >= Length (S2),
     Post => (for all E in Model (S1) => P (E));

   --  Move the content of a set into another set. The first version excludes
   --  already included element from the first set during the iteration
   --  while the other clears the first set at the end.

   procedure Move (S1, S2 : in out My_Sets.Set) with
     Pre  => Capacity (S2) >= Length (S1),
     Post => Model (S1)'Old = Model (S2) and Length (S1) = 0;

   procedure Move_2 (S1, S2 : in out My_Sets.Set) with
     Pre  => Capacity (S2) >= Length (S1),
     Post => Model (S1)'Old = Model (S2) and Length (S1) = 0;

   --  Includes 5 distinct elements in S.

   procedure Insert_5 (S : in out My_Sets.Set) with
     Pre  => Capacity (S) - 5 >= Length (S),
     Post => Length (S) <= Length (S)'Old + 5
     and Inc (Model (S)'Old, Model (S))
     and (for all E in 1 .. 5 => Mem (Model (S), E))
     and (for all E in Model (S) => Mem (Model (S)'Old, E) or E in 1 .. 5);
end Use_Sets;
