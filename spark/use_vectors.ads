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

   procedure Incr_All (V1 : Vector; V2 : in out Vector) with
     Post => Length (V2) = Length (V1)
     and (for all N in Index_Type'First .. Last (V1) =>
              Is_Incr (Element (V1, N),
                       Element (V2, N)));
   --  Loop through a vector to increment each element. Store the incremented
   --  elements in V2.

   procedure Incr_All_2 (V : in out Vector) with
     Post => Length (V) = Length (V)'Old
     and (for all N in Index_Type'First .. Last (V) =>
              Is_Incr (Element (Model (V)'Old, N),
                       Element (V, N)));
   --  Same as before except that elements are stored back in V.

   procedure Incr_All_3 (V : in out Vector) with
     Post => Length (V) = Length (V)'Old
     and (for all N in Index_Type'First .. Last (V) =>
              Is_Incr (Element (Model (V)'Old, N),
                       Element (V, N)))
     and Valid_Cursors (V)'Old = Valid_Cursors (V);
   --  Same as before except that we use cursors instead of indexes in the
   --  specification.

   procedure Double_Size (V : in out Vector) with
     Pre  => Max_Capacity / 2 >= Length (V),
     Post => Length (V) = 2 * Length (V)'Old
     and (for all I in Index_Type'First .. Last (V)'Old =>
       Element (V, I) = Element (Model (V)'Old, I)
       and Element (V, I + Length (V)'Old) =
           Element (Model (V)'Old, I));
   --  Double the size of list by duplicating every element. New elements are
   --  appended to the list.

   function My_Find (V : Vector; E : Integer) return Index_Type'Base
   --  Iterate to find an element E in V.

   with
     Contract_Cases =>
       ((for all F of V => F /= E) =>
            My_Find'Result = Index_Type'First - 1,
        others => My_Find'Result in Index_Type'First .. Last (V)
        and then Element (V, My_Find'Result) = E
        and then (for all I in Index_Type'First .. My_Find'Result - 1 =>
                  Element (V, I) /= E));

   procedure Update_Range_To_Zero (V : in out Vector; Fst, Lst : Index_Type)
   --  Replace every element between Fst and Lst with 0.

   with
     Pre  => Fst <= Last (V) and then Lst <= Last (V) and then Fst <= Lst,
     Post => Valid_Cursors (V) = Valid_Cursors (V)'Old
     and (for all I in Index_Type'First .. Last (V) =>
              (if I in Fst .. Lst
               then Element (V, I) = 0
               else Element (V, I) = Element (Model (V)'Old, I)));

   Count : constant := 7;

   procedure Insert_Count (V : in out Vector; I : Index_Type)
   --  Insert 0 Count times just before I.

   with
     Pre  => I <= Last (V) and Max_Capacity - Count >= Length (V),
     Post => Length (V) = Length (V)'Old + Count
     and (for all J in Index_Type'First .. I - 1 =>
        Element (V, J) = Element (Model (V)'Old, J))
     and (for all J in I .. I + Count - 1 => Element (V, J) = 0)
     and (for all J in I + Count .. Last (V) =>
              Element (V, J) = Element (Model (V)'Old, J - Count));

   type My_Enum_Base is (Zero, One, Two, Three);
   subtype My_Enum is My_Enum_Base range One .. Three;

   package Enum_Vectors is new
     Formal_Vectors
       (Index_Type => My_Enum, Element_Type => Integer);

   --  Test links between high level, position based model of a container and
   --  lower level, cursor based model.

   function P (E : Integer) return Boolean;
   --  Any property P on an Integer E.

   procedure From_Higher_To_Lower (V : Vector) with
     Ghost,
     Global => null,
     Pre    => (for all E of V => P (E)),
     Post   => (for all Cu in V => P (Element (V, Cu)));
   --  Test that the link can be done from a property on the elements of a
   --  high level view of a container and its low level view.

   procedure From_Lower_To_Higher (V : Vector) with
     Ghost,
     Global => null,
     Pre    => (for all Cu in V => P (Element (V, Cu))),
     Post   => (for all E of V => P (E));
   --  Test that the link can be done from a property on the elements of a
   --  low level view of a container and its high level view.
end Use_Vectors;
