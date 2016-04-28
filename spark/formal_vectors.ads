pragma Ada_2012;
with Conts; use Conts;
with Conts.Vectors.Indefinite_Unbounded_SPARK;
with Functional_Sequences;
with Functional_Sets;

generic
   type Index_Type is (<>);
   --  To avoid Constraint_Error being raised at runtime, Index_Type'Base
   --  should have at least one more element at the left than Index_Type.

   type Element_Type (<>) is private;
package Formal_Vectors with SPARK_Mode is

   subtype Extended_Index is Index_Type'Base range
     Index_Type'Pred (Index_Type'First) .. Index_Type'Last;
   --  Index_Type with one more element to the left.

   package Element_Vectors is new Conts.Vectors.Indefinite_Unbounded_SPARK
     (Index_Type          => Index_Type,
      Element_Type        => Element_Type,
      Container_Base_Type => Limited_Base);
   --  Instance of the container package. That would be better if it was
   --  instantiated in the private part but then the Cursor type could not be
   --  used to instanciate the Functional_Maps package for the Formal_Model.

   subtype Cursor is Element_Vectors.Cursor;
   use all type Element_Vectors.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Vectors.Vectors.No_Element;

   type Vector is tagged limited private with
     Default_Initial_Condition => Length (Vector) = 0;
   --  Vectors are empty when default initialized

   Max_Capacity : constant Count_Type :=
     (if Index_Type'Pos (Index_Type'Last) < Index_Type'Pos (Index_Type'First)
      then 0
      elsif Index_Type'Pos (Index_Type'Last) -
          Index_Type'Pos (Index_Type'First) + 1
      in Count_Type
      then Index_Type'Pos (Index_Type'Last) -
          Index_Type'Pos (Index_Type'First) + 1
      else Count_Type'Last);
   --  Maximal capacity of a vector. It is the minimum of the size of the
   --  index range and the last possible Count_Type.
   --  Actual capacity of a vector is not modeled. It is mostly irrelevant
   --  since it cannot be querried and it does not affect the behavior of other
   --  subprograms.

   function Length (Self : Vector'Class) return Natural with
     Global => null,
     Post   => Length'Result <= Max_Capacity;
   --  The length of a vector is always smaller than its capacity

   function Last (Self : Vector'Class) return Extended_Index with
     Global => null,
     Post   => Last'Result =
      Index_Type'Val ((Index_Type'Pos (Index_Type'First) - 1) + Length (Self));

   function To_Index (Position : Cursor) return Index_Type with
     Global => null,
     Pre    => Position /= No_Element;

   package Formal_Model is

      --  This package should be Ghost if possible. Currently, the compiler
      --  complains that the parent type of a Ghost type extension shall be
      --  Ghost (see OA30-006).

      package V is new Functional_Sets
        (Element_Type => Cursor,
         No_Element   => No_Element);
      package M is new Functional_Sequences
        (Index_Type   => Index_Type,
         Element_Type => Element_Type);

      function Model (Self : Vector'Class) return M.Sequence with
        Global => null,
        Post   => M.Length (Model'Result) = Length (Self);
      --  The highlevel model of a vector is a sequence of elements indexed by
      --  Index_Type. Cursors are not represented in this model.

      function Valid_Cursors (Self : Vector'Class) return V.Set with
      --  Valid_Cursors is the set of cursors that are valid in V.

        Global => null,
        Post   =>

          --  Positions of cursors are smaller than the container's length.

          (for all I in Valid_Cursors'Result =>
             To_Index (I) in Index_Type'First .. Last (Self)

           --  There is no more than one cursor per position in the container.

           and then
             (for all J in Valid_Cursors'Result =>
                  (if To_Index (I) = To_Index (J) then I = J)));
   end Formal_Model;

   package M renames Formal_Model.M;
   package V renames Formal_Model.V;

   use type M.Sequence;
   use type V.Set;

   function Model (Self : Vector'Class) return M.Sequence
                   renames Formal_Model.Model;
   function Element (S : M.Sequence; I : Index_Type) return Element_Type
                   renames M.Get;
   function Valid_Cursors (Self : Vector'Class) return V.Set
                   renames Formal_Model.Valid_Cursors;

   function Element
     (Self : Vector'Class; Position : Index_Type) return Element_Type
   --  Fetch the corresponding element in Model.

   with
       Global => null,
       Pre    => Position <= Last (Self),
       Post   => Element'Result = Element (Model (Self), Position);

   procedure Reserve_Capacity
     (Self : in out Vector'Class; Capacity : Count_Type)
   --  Make sure the

   with
       Global => null,
       Pre    => Capacity <= Max_Capacity,
       Post   => Length (Self) = Length (Self)'Old
     and then Model (Self) = Model (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old;

   procedure Shrink_To_Fit (Self : in out Vector'Class)
   with
       Global => null,
       Post   => Length (Self) = Length (Self)'Old
     and then Model (Self) = Model (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old;

   procedure Resize
     (Self    : in out Vector'Class;
      Length  : Index_Type;
      Element : Element_Type)
     with
       Global => null,
       Pre    =>
         Index_Type'Pos (Length) - Index_Type'Pos (Index_Type'First) + 1
           <= Max_Capacity,
       Post   => Formal_Vectors.Length (Self) =
             Index_Type'Pos (Length) - Index_Type'Pos (Index_Type'First) + 1
     and then
       (for all I in
          Index_Type'First .. Index_Type'Min (Length, Last (Self)'Old) =>
            Formal_Vectors.Element (Self, I) =
            Formal_Vectors.Element (Model (Self)'Old, I))
     and then (for all I in Last (Self)'Old .. Last (Self) =>
                   Formal_Vectors.Element (Model (Self), I) = Element);

   function Is_Empty (Self : Vector'Class) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (Length (Self) = 0);

   procedure Append
     (Self    : in out Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
     with
       Global => null,
       Pre    => Length (Self) <= Max_Capacity - Count,
       Post   => Length (Self) = Length (Self)'Old + Count
     and then
       (for all I in Index_Type'First .. Last (Self)'Old =>
            Formal_Vectors.Element (Self, I) =
            Formal_Vectors.Element (Model (Self)'Old, I))
     and then (for all I in Last (Self)'Old .. Last (Self) =>
                         Formal_Vectors.Element (Self, I) = Element)
     and then V.Inc (Valid_Cursors (Self)'Old, Valid_Cursors (Self));

   procedure Replace_Element
     (Self     : in out Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
   with
       Global => null,
       Post   => Length (Self) = Length (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old
     and then
       (if Index in Index_Type'First .. Last (Self)
        then M.Is_Replace
          (Model (Self)'Old, Index, New_Item, Model (Self))
        else Model (Self) = Model (Self)'Old);

   procedure Swap
     (Self        : in out Vector'Class;
      Left, Right : Index_Type)
     with
       Global => null,
       Pre    => Left in Index_Type'First .. Last (Self)
     and then Right in Index_Type'First .. Last (Self),
       Post   =>  Length (Self) = Length (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old
     and then
       (for all I in Index_Type'First .. Last (Self) =>
          (if I /= Left and I /= Right then
                 Element (Self, I) = Element (Model (Self)'Old, I)))
     and then Element (Self, Left) =
       Element (Model (Self)'Old, Right)
     and then Element (Self, Right) =
       Element (Model (Self)'Old, Left);

   procedure Clear (Self : in out Vector'Class) with
     Global => null,
     Post   => Length (Self) = 0;

   procedure Delete (Self : in out Vector'Class; Index : Index_Type)
   with
       Global => null,
       Pre    => Index in Index_Type'First .. Last (Self),
       Post   => Length (Self) = Length (Self)'Old - 1
     and then
       (for all I in Index_Type'First .. Index_Type'Pred (Index) =>
              Element (Self, I) = Element (Model (Self)'Old, I))
     and then
       (for all I in Index .. Last (Self) =>
              Element (Self, I) =
          Element (Model (Self)'Old, Index_Type'Succ (I)));

   procedure Delete_Last (Self : in out Vector'Class) with
     Global => null,
     Pre    => Length (Self) > 0,
     Post   => Length (Self) = Length (Self)'Old - 1
     and then
       (for all I in Index_Type'First .. Last (Self) =>
              Element (Self, I) = Element (Model (Self)'Old, I));

   function Last_Element (Self : Vector'Class) return Element_Type with
     Global => null,
     Pre    => Length (Self) > 0,
     Post   =>
       Last_Element'Result = Element (Self, Last (Self));

   procedure Assign
     (Self : in out Vector'Class; Source : Vector'Class)
   with
       Global => null,
       Post   => Length (Self) = Length (Source)
     and then
       (for all I in Index_Type'First .. Last (Self) =>
              Element (Self, I) = Element (Source, I));

   function First (Self : Vector'Class) return Cursor with
     Global => null,
     Post   => (if Length (Self) = 0 then First'Result = No_Element
              else To_Index (First'Result) = Index_Type'First
                and then V.Mem (Valid_Cursors (Self), First'Result));
   function Element
     (Self : Vector'Class; Position : Cursor) return Element_Type
   with
     Global => null,
     Pre    => V.Mem (Valid_Cursors (Self), Position),
     Post   => Element'Result = Element (Self, To_Index (Position));
   function Has_Element
     (Self : Vector'Class; Position : Cursor) return Boolean
   with
       Global => null,
       Post   => Has_Element'Result = V.Mem (Valid_Cursors (Self), Position);
   function Next
     (Self : Vector'Class; Position : Cursor) return Cursor
     with
       Global => null,
       Post   =>
       (if To_Index (Position) < Last (Self)
        and then V.Mem (Valid_Cursors (Self), Position)
        then To_Index (Next'Result) = Index_Type'Succ (To_Index (Position))
        and V.Mem (Valid_Cursors (Self), Position)
        else Next'Result = No_Element);
   function Previous
     (Self : Vector'Class; Position : Cursor) return Cursor
     with
       Global => null,
       Post   =>
       (if To_Index (Position) > Index_Type'First
        and then V.Mem (Valid_Cursors (Self), Position)
        then To_Index (Previous'Result) = Index_Type'Pred (To_Index (Position))
        and V.Mem (Valid_Cursors (Self), Position)
        else Previous'Result = No_Element);

   procedure Next (Self : Vector'Class; Position : in out Cursor)
      with Inline,
           Global => null,
           Pre    => Has_Element (Self, Position),
       Post   =>
       (if To_Index (Position)'Old < Last (Self)
        then To_Index (Position) = Index_Type'Succ (To_Index (Position)'Old)
        and V.Mem (Valid_Cursors (Self), Position)
        else Position = No_Element);

private
   pragma SPARK_Mode (Off);

   type Vector is new Element_Vectors.Vector with null record;

end Formal_Vectors;
