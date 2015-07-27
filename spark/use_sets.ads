pragma Ada_2012;
with Formal_Hashed_Sets;
pragma Elaborate_All (Formal_Hashed_Sets);

package Use_Sets with SPARK_Mode is
   package My_Sets is new Formal_Hashed_Sets
     (Element_Type => Integer,
      "="          => "=",
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

   function My_Find (S : My_Sets.Set; E : Integer) return Cursor with
     Post => My_Find'Result = Find (S, E);

   function F (E : Integer) return Integer is
      (if E in -100 .. 100 then E * 2 else E);

   procedure Apply_F (S : My_Sets.Set; R : in out My_Sets.Set) with
     Pre  => Capacity (R) >= Length (S),
     Post => Capacity (R) = Capacity (R)'Old
     and (for all E in Model (S) => Mem (Model (R), F (E)))
     and (for all G in Model (R) =>
              (for some E in Model (S) => G = F (E)));

   function Are_Disjoint (S1, S2 : My_Sets.Set) return Boolean with
     Post => Are_Disjoint'Result =
       Is_Empty (Intersection (Model (S1), Model (S2)));

   function Are_Disjoint_2 (S1, S2 : My_Sets.Set) return Boolean with
     Post => Are_Disjoint_2'Result =
       (for all E in Model (S2) => not Mem (Model (S1), E));

   function P (E : Integer) return Boolean is
     (E >= 0);

   procedure Union_P (S1 : in out My_Sets.Set; S2 : My_Sets.Set) with
     Pre  => (for all E in Model (S1) => P (E))
     and (for all E in Model (S2) => P (E))
     and Capacity (S1) - Length (S1) >= Length (S2),
     Post => (for all E in Model (S1) => P (E));
end Use_Sets;
