pragma Ada_2012;
with Conts.Vectors.Indefinite_Unbounded_SPARK;
with Conts.Vectors.Definite_Bounded;
with Conts;              use Conts;
with Conts.Algorithms.SPARK;
pragma Elaborate_All (Conts.Vectors.Indefinite_Unbounded_SPARK);

package Use_Vectors with SPARK_Mode is
   subtype Smaller is Integer range Integer'First + 1 .. Integer'Last;

   package Nested is
      Lst : constant Integer;
   private
      pragma SPARK_Mode (Off);
      function Id (X : Integer) return Integer is (X);
      Lst : constant Integer := Id (Integer'Last);
   end Nested;

   First_Index : constant Smaller := 1;
   Last_Index  : constant Integer := Nested.Lst;

   subtype Index_Type is Integer range First_Index .. Last_Index;

   type Element_Type is new Integer;
   package My_Vectors is new
     Conts.Vectors.Indefinite_Unbounded_SPARK
       (Index_Type => Index_Type,
        Element_Type => Element_Type);
   package My_Bounded_Vectors is new
     Conts.Vectors.Definite_Bounded
       (Index_Type => Index_Type,
        Element_Type => Element_Type);
   use type My_Vectors.Cursor;
   use My_Vectors.Vectors;
   use type My_Vectors.Element_Sequence;
   use all type My_Vectors.Cursor_Set;

   subtype My_Bounded is My_Bounded_Vectors.Vector;
   subtype My_Bounded_100 is My_Bounded_Vectors.Vector (100);

   pragma Unevaluated_Use_Of_Old (Allow);

   function Find is new Conts.Algorithms.SPARK.Find
     (Cursors => My_Vectors.Cursors.Forward,
      Getters => My_Vectors.Maps.Constant_Returned,
      "="     => "=",
      Content => My_Vectors.Content_Models);

   function Is_Incr (I1, I2 : Element_Type) return Boolean is
      (if I1 = Element_Type'Last then I2 = Element_Type'Last else I2 = I1 + 1);

   procedure Incr_All (V1 : My_Bounded_100; V2 : in out My_Bounded_100) with
     Post => My_Bounded_Vectors.Vectors.Length (V2) =
     My_Bounded_Vectors.Vectors.Length (V1)
     and (for all N in Index_Type'First ..
            My_Bounded_Vectors.Vectors.Last (V1) =>
              Is_Incr (My_Bounded_Vectors.Vectors.As_Element (V1, N),
                       My_Bounded_Vectors.Vectors.As_Element (V2, N)));
   --  Loop through a vector to increment each element. Store the incremented
   --  elements in V2. This test uses bounded vectors.

   procedure Incr_All_2 (V : in out Vector) with
     Post => Length (V) = Length (V)'Old
     and (for all N in Index_Type'First .. Last (V) =>
              Is_Incr (Element (Model (V)'Old, N),
                       As_Element (V, N)));
   --  Same as before except that elements are stored back in V.

   procedure Incr_All_3 (V : in out Vector) with
     Post => Length (V) = Length (V)'Old
     and (for all N in Index_Type'First .. Last (V) =>
              Is_Incr (Element (Model (V)'Old, N),
                       As_Element (V, N)))
     and Valid_Cursors (V)'Old = Valid_Cursors (V);
   --  Same as before except that we use cursors instead of indexes in the
   --  specification.

   procedure Double_Size (V : in out Vector) with
     Pre  => Impl.Last_Count / 2 >= Length (V),
     Post => Length (V) = 2 * Length (V)'Old
     and (for all I in Index_Type'First .. Last (V)'Old =>
       As_Element (V, I) = Element (Model (V)'Old, I)
       and As_Element (V, I + Integer (Length (V)'Old)) =
           Element (Model (V)'Old, I));
   --  Double the size of list by duplicating every element. New elements are
   --  appended to the list.

   function My_Find (V : Vector; E : Element_Type) return Index_Type'Base
   --  Iterate to find an element E in V.

   with
     Contract_Cases =>
       ((for all F of V => F /= E) =>
            My_Find'Result = Index_Type'First - 1,
        others => My_Find'Result in Index_Type'First .. Last (V)
        and then As_Element (V, My_Find'Result) = E
        and then (for all I in Index_Type'First .. My_Find'Result - 1 =>
                  As_Element (V, I) /= E));

   procedure Update_Range_To_Zero (V : in out Vector; Fst, Lst : Index_Type)
   --  Replace every element between Fst and Lst with 0.

   with
     Pre  => Lst <= Last (V) and then Fst <= Lst,
     Post => Valid_Cursors (V) = Valid_Cursors (V)'Old
     and (for all I in Index_Type'First .. Last (V) =>
              (if I in Fst .. Lst
               then As_Element (V, I) = 0
               else As_Element (V, I) = Element (Model (V)'Old, I)));

   Count : constant := 7;

   procedure Insert_Count (V : in out Vector; I : Index_Type)
   --  Insert 0 Count times just before I.

   with
     Pre  => I <= Last (V) and Impl.Last_Count - Count >= Length (V),
     Post => Length (V) = Length (V)'Old + Count
     and (for all J in Index_Type'First .. I - 1 =>
        As_Element (V, J) = Element (Model (V)'Old, J))
     and (for all J in I .. I + Count - 1 => As_Element (V, J) = 0)
     and (for all J in I + Count .. Last (V) =>
              As_Element (V, J) = Element (Model (V)'Old, J - Count));

   type My_Enum_Base is (Zero, One, Two, Three);
   subtype My_Enum is My_Enum_Base range One .. Three;

   package Enum_Vectors is new
     Conts.Vectors.Indefinite_Unbounded_SPARK
       (Index_Type => My_Enum, Element_Type => Integer);

   --  Test links between high level, position based model of a container and
   --  lower level, cursor based model.

   function P (E : Element_Type) return Boolean;
   --  Any property P on an Integer E.

   procedure From_Higher_To_Lower (V : Vector) with
     Ghost,
     Global => null,
     Pre    => (for all E of V => P (E)),
     Post   => (for all Cu in V => P (As_Element (V, Cu)));
   --  Test that the link can be done from a property on the elements of a
   --  high level view of a container and its low level view.

   procedure From_Lower_To_Higher (V : Vector) with
     Ghost,
     Global => null,
     Pre    => (for all Cu in V => P (As_Element (V, Cu))),
     Post   => (for all E of V => P (E));
   --  Test that the link can be done from a property on the elements of a
   --  low level view of a container and its high level view.
end Use_Vectors;
