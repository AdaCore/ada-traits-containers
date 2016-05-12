pragma Ada_2012;
with Conts; use Conts;
with Conts.Vectors.Indefinite_Unbounded_SPARK;
with Functional_Sequences;
with Functional_Sets;

generic
   type Index_Type is (<>);
   --  To avoid Constraint_Error being raised at runtime, Index_Type'Base
   --  should have at least one more element to the left than Index_Type.

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
     Default_Initial_Condition => Length (Vector) = 0,
     Iterable => (First       => First_Primitive,
                  Next        => Next_Primitive,
                  Has_Element => Has_Element_Primitive,
                  Element     => Element_Primitive);
   --  Vectors are empty when default initialized.
   --  Iteration over vectors can be done over cursors or over elements.

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
   --  ??? Actual capacity of a vector is not modeled. Do we want to add it?

   function Length (Self : Vector'Class) return Natural with
     Global => null,
     Post   => Length'Result <= Max_Capacity;
   --  The length of a vector is always smaller than Max_Capacity.

   function Last (Self : Vector'Class) return Extended_Index with
     Global => null,
     Post   => Last'Result =
       Index_Type'Val ((Index_Type'Pos (Index_Type'First) - 1)
                       + Length (Self));
   --  On an empty vector, Last returns Extended_Index'First.

   function To_Index (Position : Cursor) return Index_Type with
     Global => null,
     Pre    => Position /= No_Element;
   --  In vectors, cursors cannot change positions. They are associated with a
   --  unique index by To_Index.

   package Formal_Model with Ghost is

      package V is new Functional_Sets
        (Element_Type => Cursor);
      package M is new Functional_Sequences
        (Index_Type   => Index_Type,
         Element_Type => Element_Type);

      function Model (Self : Vector'Class) return M.Sequence with
        Global => null,
        Post   => M.Length (Model'Result) = Length (Self);
      --  The highlevel model of a vector is a sequence of elements indexed by
      --  Index_Type. Cursors are not represented in this model.

      pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

      function Valid_Cursors (Self : Vector'Class) return V.Set with
      --  Valid_Cursors is the set of cursors that are valid in Self.
      --  No need to store their position in Self as it can be retrieved with
      --  To_Index.

        Global => null,
        Post   => not V.Mem (Valid_Cursors'Result, No_Element)

        --  Positions of cursors are smaller than the container's last index.

        and then
          (for all I of Valid_Cursors'Result =>
             To_Index (I) in Index_Type'First .. Last (Self)

           --  There is no more than one cursor per position in the container.

           and then
             (for all J of Valid_Cursors'Result =>
                  (if To_Index (I) = To_Index (J) then I = J)));

      procedure Lift_Abstraction_Level (Self : Vector'Class) with
        Global => null,
        Post   =>
          (for all Elt of Model (Self) =>
             (for some I of Valid_Cursors (Self) =>
                  M.Get (Model (Self), To_Index (I)) = Elt));
      --  Lift_Abstraction_Level is a ghost procedure that does nothing but
      --  assume that we can access to the same elements by iterating over
      --  positions or cursors.
      --  This information is not generally useful except when switching from
      --  a lowlevel, cursor aware view of a container, to a highlevel position
      --  based view.
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

   --  The following functions are modeled directly in the formal model.

   function Element
     (Self : Vector'Class; Position : Index_Type) return Element_Type
   --  Fetch the corresponding element in Model.

   with
       Global => null,
       Pre    => Position <= Last (Self),
       Post   => Element'Result = Element (Model (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element);

   function Has_Element
     (Self : Vector'Class; Position : Cursor) return Boolean
   with
       Global => null,
       Post   => Has_Element'Result = V.Mem (Valid_Cursors (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Element
     (Self : Vector'Class; Position : Cursor) return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Self, Position),
     Post   => Element'Result = Element (Model (Self), To_Index (Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Element);

   function Last_Element (Self : Vector'Class) return Element_Type with
     Global => null,
     Pre    => Length (Self) > 0,
     Post   => Last_Element'Result = Element (Self, Last (Self));

   procedure Reserve_Capacity
     (Self : in out Vector'Class; Capacity : Count_Type)
   --  Make sure there is enough space for at least Count_Type elements in
   --  Self.

   with
       Global => null,
       Pre    => Capacity <= Max_Capacity,
       Post   => Length (Self) = Length (Self)'Old
     and then Model (Self) = Model (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old;

   procedure Shrink_To_Fit (Self : in out Vector'Class)
   --  Resize the vector to fit its number of elements.
   --  This has no effect on models.
   --  ??? are cursors preserved?

   with
       Global => null,
       Post   => Length (Self) = Length (Self)'Old
     and then Model (Self) = Model (Self)'Old;

   procedure Resize
     (Self    : in out Vector'Class;
      Length  : Index_Type;
      Element : Element_Type)
   --  Resize the container so that it contains Length elements.
   --  If Length is smaller than the current container length, Self is
   --     reduced to its first Length elements, destroying the other elements.
   --  If Length is greater than the current container length, new elements
   --     are added as needed, as copied of Element.
   --  ??? are cursors preserved?

   with
     Global => null,
     Pre    =>
         Index_Type'Pos (Length) - Index_Type'Pos (Index_Type'First) + 1
             <= Max_Capacity,
     Post   => Formal_Vectors.Length (Self) =
             Index_Type'Pos (Length) - Index_Type'Pos (Index_Type'First) + 1

     --  Elements of Self that were located before the index Length are
     --  preserved.

     and then
       (for all I in
          Index_Type'First .. Index_Type'Min (Length, Last (Self)'Old) =>
            Formal_Vectors.Element (Self, I) =
            Formal_Vectors.Element (Model (Self)'Old, I))

     --  If elements were appended to Self then they are equal to Element.

     and then (for all I in Last (Self)'Old .. Last (Self) =>
                   Formal_Vectors.Element (Self, I) = Element);

   function Is_Empty (Self : Vector'Class) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (Length (Self) = 0);

   procedure Append
     (Self    : in out Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
   --  Append Count copies of Element to the vector.
   --  ??? are cursors preserved?

   with
     Global => null,
     Pre    => Length (Self) <= Max_Capacity - Count,
     Post   => Length (Self) = Length (Self)'Old + Count

     --  Elements that were already in Self are preserved.

     and then
       (for all I in Index_Type'First .. Last (Self)'Old =>
            Formal_Vectors.Element (Self, I) =
            Formal_Vectors.Element (Model (Self)'Old, I))

     --  Appended elements are equal to Element.

     and then (for all I in Last (Self)'Old .. Last (Self) =>
                         Formal_Vectors.Element (Self, I) = Element);

   procedure Replace_Element
     (Self     : in out Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
   --  Replace the element at the given position.
   --  Nothing is done if Index is not a valid index in the container.

   with
     Global => null,
     Post   => Length (Self) = Length (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old
     and then
       (if Index <= Last (Self)
        then M.Is_Replace (Model (Self)'Old, Index, New_Item, Model (Self))
        else Model (Self) = Model (Self)'Old);

   procedure Swap
     (Self        : in out Vector'Class;
      Left, Right : Index_Type)
   --  Efficiently swap the elements at the two positions.

   with
     Global => null,
     Pre    => Left <= Last (Self) and then Right <= Last (Self),
     Post   => Length (Self) = Length (Self)'Old
     and then Element (Self, Left) = Element (Model (Self)'Old, Right)
     and then Element (Self, Right) = Element (Model (Self)'Old, Left)

     --  Valid cursors are preserved

     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old

     --  Elements that have not been swapped are preserved.

     and then
       (for all I in Index_Type'First .. Last (Self) =>
          (if I /= Left and I /= Right then
                 Element (Self, I) = Element (Model (Self)'Old, I)));

   procedure Clear (Self : in out Vector'Class) with
     Global => null,
     Post   => Length (Self) = 0;

   procedure Delete (Self : in out Vector'Class; Index : Index_Type)
   --  Remove an element from the vector.
   --  ??? are cursors preserved?

   with
     Global => null,
     Pre    => Index <= Last (Self),
     Post   => Length (Self) = Length (Self)'Old - 1

     --  Elements located before Index are preserved.

     and then
       (for all I in Index_Type'First .. Index_Type'Pred (Index) =>
              Element (Self, I) = Element (Model (Self)'Old, I))

     --  Elements located after Index are shifted.

     and then
       (for all I in Index .. Last (Self) =>
              Element (Self, I) =
          Element (Model (Self)'Old, Index_Type'Succ (I)));

   procedure Delete_Last (Self : in out Vector'Class) with
   --  Remove the last element of the vector.
   --  ??? are cursors preserved?

     Global => null,
     Pre    => Length (Self) > 0,
     Post   => Length (Self) = Length (Self)'Old - 1
     and then (for all I in Index_Type'First .. Last (Self) =>
                   Element (Self, I) = Element (Model (Self)'Old, I));

   procedure Assign
     (Self : in out Vector'Class; Source : Vector'Class)
   with
       Global => null,
       Post   => Model (Self) = Model (Source);

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : Vector'Class) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Self) = 0 => First'Result = No_Element,
        others            => To_Index (First'Result) = Index_Type'First
        and then Has_Element (Self, First'Result));

   function Next
     (Self : Vector'Class; Position : Cursor) return Cursor
   with
     Global           => null,
     Pre              => Has_Element (Self, Position),
     Contract_Cases   =>
       (To_Index (Position) < Last (Self) =>
              To_Index (Next'Result) = Index_Type'Succ (To_Index (Position))
        and Has_Element (Self, Next'Result),
        others                            => Next'Result = No_Element);

   function Previous
     (Self : Vector'Class; Position : Cursor) return Cursor
   with
     Global           => null,
     Pre              => Has_Element (Self, Position),
     Contract_Cases   =>
       (To_Index (Position) > Index_Type'First =>
             To_Index (Previous'Result) = Index_Type'Pred (To_Index (Position))
        and Has_Element (Self, Previous'Result),
        others                            => Previous'Result = No_Element);

   procedure Next (Self : Vector'Class; Position : in out Cursor) with
     Global           => null,
     Pre              => Has_Element (Self, Position),
     Contract_Cases   =>
       (To_Index (Position) < Last (Self) =>
              To_Index (Position) = Index_Type'Succ (To_Index (Position'Old))
        and Has_Element (Self, Position),
        others                            => Position = No_Element);

   function First_Primitive (Self : Vector) return Cursor;

   function Element_Primitive
     (Self : Vector; Position : Cursor) return Element_Type
   with
     Inline,
     Pre'Class => Has_Element (Self, Position),
     Post      => Element_Primitive'Result =
       Element (Model (Self), To_Index (Position));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element_Primitive);

   function Has_Element_Primitive
     (Self : Vector; Position : Cursor) return Boolean
   with Post =>
       Has_Element_Primitive'Result = V.Mem (Valid_Cursors (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element_Primitive);

   function Next_Primitive
     (Self : Vector; Position : Cursor) return Cursor
   with
     Inline,
     Pre'Class => Has_Element (Self, Position);

private
   pragma SPARK_Mode (Off);

   type Vector is new Element_Vectors.Vector with null record;

end Formal_Vectors;
