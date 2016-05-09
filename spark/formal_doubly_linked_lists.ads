pragma Ada_2012;
with Conts.Lists.Indefinite_Unbounded_SPARK;
with Functional_Sequences;
with Functional_Maps;
with Conts; use Conts;

generic
   type Element_Type (<>) is private;
package Formal_Doubly_Linked_Lists with SPARK_Mode is

   package Element_Lists is new Conts.Lists.Indefinite_Unbounded_SPARK
     (Element_Type => Element_Type);
   --  Instance of the container package. That would be better if it was
   --  instantiated in the private part but then the Cursor type could not be
   --  used to instanciate the Functional_Maps package for the Formal_Model.

   subtype Cursor is Element_Lists.Cursor;
   use all type Element_Lists.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Lists.Lists.No_Element;

   type List is tagged limited private with
     Default_Initial_Condition => Length (List) = 0,
     Iterable => (First       => First_Primitive,
                  Next        => Next_Primitive,
                  Has_Element => Has_Element_Primitive,
                  Element     => Element_Primitive);
   --  Lists are empty when default initialized

   function Capacity (Self : List'Class) return Natural;

   function Length (Self : List'Class) return Natural with
     Post => Length'Result <= Capacity (Self);
   --  The length of a list is always smaller than its capacity

   package Formal_Model is

      --  This package should be Ghost if possible. Currently, the compiler
      --  complains that the parent type of a Ghost type extension shall be
      --  Ghost (see OA30-006).

      package P is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor);
      package M is new Functional_Sequences
        (Index_Type   => Positive,
         Element_Type => Element_Type);

      function Model (Self : List'Class) return M.Sequence with
        Post => M.Length (Model'Result) = Length (Self);
      --  The highlevel model of a list is a sequence of elements. Cursors are
      --  not represented in this model.

      pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

      function Positions (Self : List'Class) return P.Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and map them to their position in the container.

        Post => not P.Mem (Positions'Result, No_Element)

          --  Positions of cursors are smaller than the container's length.

          and then
            (for all I of Positions'Result =>
               P.Get (Positions'Result, I) in 1 .. Length (Self)

           --  No two cursors have the same position. Note that we do not state
           --  that there is a cursor in the map for each position, as it is
           --  rarely needed.

           and then
             (for all J of Positions'Result =>
                (if P.Get (Positions'Result, I) =
                     P.Get (Positions'Result, J)
                       then I = J)));

      procedure Lift_Abstraction_Level (Self : List'Class) with
        Global => null,
        Post   =>
          (for all I in 1 .. Length (Self) =>
             (for some Position of Positions (Self) =>
                  M.Get (Model (Self), P.Get (Positions (Self), Position)) =
                M.Get (Model (Self), I)));
      --  Lift_Abstraction_Level is a ghost procedure that does nothing but
      --  assume that we can access to the same elements by iterating over
      --  positions or cursors.
      --  This information is not generally useful except when switching from
      --  a lowlevel, cursor aware view of a container, to a highlevel position
      --  based view.
   end Formal_Model;

   package M renames Formal_Model.M;
   package P renames Formal_Model.P;

   use type M.Sequence;
   use type P.Map;

   function Model (Self : List'Class) return M.Sequence
                   renames Formal_Model.Model;
   function Element (S : M.Sequence; I : Positive) return Element_Type
                   renames M.Get;
   function Positions (Self : List'Class) return P.Map
                   renames Formal_Model.Positions;

   function Element (Self : List'Class; Position : Cursor) return Element_Type
   with
     Pre  => P.Mem (Positions (Self), Position),
     Post =>

       --  Query Positions to get the position of Position in Self and use it
       --  to fetch the corresponding element in Model.

     Element'Result = Element (Model (Self),
                               P.Get (Positions (Self), Position));

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : List'Class) return Cursor with
     Post => (if Length (Self) = 0 then First'Result = No_Element
              else P.Mem (Positions (Self), First'Result) and then
                  P.Get (Positions (Self), First'Result) = 1);

   procedure Next (Self : List'Class; Position : in out Cursor) with
     Pre  => P.Mem (Positions (Self), Position),
     Post => (if P.Get (Positions (Self), Position'Old) = Length (Self)
              then Position = No_Element
              else P.Mem (Positions (Self), Position)
                and then P.Get (Positions (Self), Position) =
                  P.Get (Positions (Self), Position'Old) + 1);

   function Next (Self : List'Class; Position : Cursor) return Cursor with
     Pre  => P.Mem (Positions (Self), Position),
     Post => (if P.Get (Positions (Self), Position) = Length (Self)
              then Next'Result = No_Element
              else P.Mem (Positions (Self), Next'Result)
                and then P.Get (Positions (Self), Next'Result) =
                  P.Get (Positions (Self), Position) + 1);

   function Last (Self : List'Class) return Cursor with
     Import,
     Post => (if Length (Self) = 0 then Last'Result = No_Element
              else P.Mem (Positions (Self), Last'Result) and then
                  P.Get (Positions (Self), Last'Result) = Length (Self));

   function Has_Element (Self : List'Class; Position : Cursor) return Boolean
   with
     Post => Has_Element'Result = P.Mem (Positions (Self), Position);

   function Find (Self : List'Class; Element : Element_Type) return Cursor with
     Import,
     Post =>

       --  Either Element is not in the model and the result is No_Element

       (Find'Result = No_Element
        and (for all I in 1 .. Length (Self) =>
            Formal_Doubly_Linked_Lists.Element (Model (Self), I) /= Element))

     --  or the result is a valid cursor, Element is stored at its position in
     --  Self and there is no previous occurrence of Element in L.

     or else
       (P.Mem (Positions (Self), Find'Result)
        and Formal_Doubly_Linked_Lists.Element
          (Model (Self), P.Get (Positions (Self), Find'Result)) = Element
        and (for all I in 1 .. P.Get (Positions (Self), Find'Result) - 1 =>
               Formal_Doubly_Linked_Lists.Element (Model (Self), I)
                /= Element));

   procedure Append (Self : in out List'Class; Element : Element_Type) with
     Pre  => Length (Self) < Count_Type'Last,
     Post => (if Length (Self)'Old < Capacity (Self)'Old
              then Capacity (Self) = Capacity (Self)'Old
              else Capacity (Self) > Capacity (Self)'Old)
     and Length (Self) = Length (Self)'Old + 1

     --  Positions contains a new mapping from the last cursor of Self to
     --  Length.

     and P.Is_Add
       (Positions (Self)'Old, Last (Self), Length (Self), Positions (Self))

     --  Model contains a new element Element at the end.

     and M.Is_Add (Model (Self)'Old, Element, Model (Self));

   procedure Insert
     (Self : in out List'Class; Position : Cursor; Element : Element_Type)
   with
   --  Insert Element before the valid cursor Position in L.

     Import,
     Pre  => Length (Self) < Count_Type'Last
     and then Has_Element (Self, Position),
     Post => (if Length (Self)'Old < Capacity (Self)'Old
              then Capacity (Self) = Capacity (Self)'Old
              else Capacity (Self) > Capacity (Self)'Old)
     and Length (Self) = Length (Self)'Old + 1

     --  Every cursor previously valid in Self is still valid.

     and (for all D of Positions (Self)'Old =>
              P.Mem (Positions (Self), D) and

            --  If it was located before Position in Self its position is
            --  preserved.

              (if P.Get (Positions (Self)'Old, D) <
                   P.Get (Positions (Self)'Old, Position)
               then P.Get (Positions (Self), D) =
                   P.Get (Positions (Self)'Old, D)

               --  Otherwise it is shifted by 1.

               else P.Get (Positions (Self), D) =
                 P.Get (Positions (Self)'Old, D) + 1))

     --  Every cursor valid in Self was previously valid except for the newly
     --  inserted cursor.

     and (for all D of Positions (Self) =>
              P.Mem (Positions (Self)'Old, D) or
            P.Get (Positions (Self), D) =
              P.Get (Positions (Self)'Old, Position))

     --  The elements of Self before Position are preserved.

     and (for all I in 1 .. P.Get (Positions (Self)'Old, Position) - 1 =>
              Formal_Doubly_Linked_Lists.Element (Model (Self), I) =
              Formal_Doubly_Linked_Lists.Element (Model (Self)'Old, I))

     --  Other elements are shifted by 1.

     and (for all I in
            P.Get (Positions (Self)'Old, Position) + 1 .. Length (Self) =>
              Formal_Doubly_Linked_Lists.Element (Model (Self), I) =
            Formal_Doubly_Linked_Lists. Element (Model (Self)'Old, I - 1))

     --  Element is stored at the previous position of Position in L.

     and Formal_Doubly_Linked_Lists.Element
       (Model (Self), P.Get (Positions (Self)'Old, Position)) = Element;

   procedure Replace_Element
     (Self : in out List'Class; Position : Cursor; Element : Element_Type) with
     Import,
     Pre  => Has_Element (Self, Position),
     Post => Capacity (Self) = Capacity (Self)'Old
     and Length (Self) = Length (Self)'Old

     --  Cursors are preserved.

     and Positions (Self)'Old = Positions (Self)

     --  The element at the position of Position in Self is replaced by E.

     and M.Is_Replace (Model (Self)'Old,
                       P.Get (Positions (Self), Position),
                       Element,
                       Model (Self));

   procedure Clear (Self : in out List'Class)
   with
       Post => Capacity (Self) = Capacity (Self)'Old
     and then Length (Self) = 0;

   function First_Primitive (Self : List) return Cursor;
   function Element_Primitive
     (Self : List; Position : Cursor) return Element_Type
   with
     Inline,
     Pre'Class => P.Mem (Positions (Self), Position),
     Post'Class => P.Mem (Positions (Self), Position)
           and then Element_Primitive'Result =
             Element (Model (Self), P.Get (Positions (Self), Position));
   function Has_Element_Primitive
     (Self : List; Position : Cursor) return Boolean
   with Post'Class =>
       Has_Element_Primitive'Result = P.Mem (Positions (Self), Position);
   function Next_Primitive
     (Self : List; Position : Cursor) return Cursor
   with
     Inline,
     Pre'Class => P.Mem (Positions (Self), Position);

private
   pragma SPARK_Mode (Off);

   type List is new Element_Lists.List with null record;

end Formal_Doubly_Linked_Lists;
