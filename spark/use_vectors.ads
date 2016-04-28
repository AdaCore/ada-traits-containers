pragma Ada_2012;
with Formal_Vectors;
pragma Elaborate_All (Formal_Vectors);

package Use_Vectors with SPARK_Mode is
   subtype Smaller is Integer range Integer'First + 1 .. Integer'Last;
   First_Index : Smaller := 1;
   Last_Index  : Integer := Integer'Last;
   --  First and Last of Index_Type. Those are constants but are declared as
   --  variables so that SPARK does not know their value.

   subtype Index_Type is Integer range First_Index .. Last_Index;

   package My_Vectors is new
     Formal_Vectors
       (Index_Type => Index_Type, Element_Type => Integer);
   use My_Vectors;
   use type My_Vectors.Cursor;
   use My_Vectors.V;
   use My_Vectors.M;

   pragma Unevaluated_Use_Of_Old (Allow);

   function Is_Incr (I1, I2 : Integer) return Boolean is
      (if I1 = Integer'Last then I2 = Integer'Last else I2 = I1 + 1);

   --  Incr_All loops through a vector to increment each element.
   --  The first version puts the incremented elements in V2 whereas the two
   --  others modify V. The difference is that the third one uses cursors
   --  instead of indexes.

   procedure Incr_All (V1 : Vector; V2 : in out Vector) with
     Post => Length (V2) = Length (V1)
     and (for all N in Index_Type'First .. Last (V1) =>
              Is_Incr (Element (V1, N),
                       Element (V2, N)));

   procedure Incr_All_2 (V : in out Vector) with
     Post => Length (V) = Length (V)'Old
     and (for all N in Index_Type'First .. Last (V) =>
              Is_Incr (Element (Model (V)'Old, N),
                       Element (V, N)));

   procedure Incr_All_3 (V : in out Vector) with
     Post => Length (V) = Length (V)'Old
     and (for all N in Index_Type'First .. Last (V) =>
              Is_Incr (Element (Model (V)'Old, N),
                       Element (V, N)))
     and Valid_Cursors (V)'Old = Valid_Cursors (V);

   --  Double_Size double the size of a vector by duplicating every element.

   procedure Double_Size (V : in out Vector) with
     Pre  => Max_Capacity / 2 >= Length (V),
     Post => Length (V) = 2 * Length (V)'Old
     and (for all I in Index_Type'First .. Last (V)'Old =>
       Element (V, I) = Element (Model (V)'Old, I)
       and Element (V, I + Length (V)'Old) =
           Element (Model (V)'Old, I));

   --  My_Find iterates to find an element.

   function My_Find (V : Vector; E : Integer) return Index_Type'Base with
     Contract_Cases =>
       ((for all I in Index_Type'First .. Last (V) =>
          Element (Model (V), I) /= E) =>
            My_Find'Result = Index_Type'First - 1,
        others => My_Find'Result in Index_Type'First .. Last (V)
        and then Element (Model (V), My_Find'Result) = E
        and then (for all I in Index_Type'First .. My_Find'Result - 1 =>
                  Element (Model (V), I) /= E));

   --  Update_Range_To_Zero replaces every element between Fst and Lst with 0.

   procedure Update_Range_To_Zero (V : in out Vector; Fst, Lst : Index_Type)
   with
     Pre  => Fst <= Last (V) and then Lst <= Last (V) and then Fst <= Lst,
     Post => Valid_Cursors (V) = Valid_Cursors (V)'Old
     and (for all I in Index_Type'First .. Last (V) =>
              (if I in Fst .. Lst
               then Element (Model (V), I) = 0
               else Element (Model (V), I) = Element (Model (V)'Old, I)));

   --  Insert_5 inserts 0 5 times just before I.

   procedure Insert_5 (V : in out Vector; I : Index_Type) with
     Pre  => I <= Last (V) and Max_Capacity - 5 >= Length (V),
     Post => Length (V) = Length (V)'Old + 5
     and (for all J in Index_Type'First .. I - 1 =>
        Element (Model (V), J) = Element (Model (V)'Old, J))
     and (for all J in I .. I + 4 => Element (Model (V), J) = 0)
     and (for all J in I + 5 .. Last (V) =>
              Element (Model (V), J) = Element (Model (V)'Old, J - 5));

   type My_Enum_Base is (Zero, One, Two, Three);
   subtype My_Enum is My_Enum_Base range One .. Three;

   package Enum_Vectors is new
     Formal_Vectors
       (Index_Type => My_Enum, Element_Type => Integer);
end Use_Vectors;
