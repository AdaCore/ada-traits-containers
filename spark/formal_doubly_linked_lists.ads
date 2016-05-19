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
   --  Lists are empty when default initialized.
   --  Iteration over lists can be done over cursors or over elements.

   function Capacity (Self : List'Class) return Count_Type;

   function Length (Self : List'Class) return Count_Type with
     Post => Length'Result <= Capacity (Self);
   --  The length of a list is always smaller than its capacity

   package Formal_Model with Ghost is

      package P is new Functional_Maps
        (Element_Type => Count_Type,
         Key_Type     => Cursor);
      package M is new Functional_Sequences
        (Index_Type   => Count_Type,
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
                (if P.Get (Positions'Result, I) = P.Get (Positions'Result, J)
                 then I = J)));

      procedure Lift_Abstraction_Level (Self : List'Class) with
        Global => null,
        Post   =>
          (for all Elt of Model (Self) =>
             (for some I of Positions (Self) =>
                  M.Get (Model (Self), P.Get (Positions (Self), I)) = Elt));
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
   function Element (S : M.Sequence; I : Count_Type) return Element_Type
                   renames M.Get;
   function Positions (Self : List'Class) return P.Map
                   renames Formal_Model.Positions;

   --  The following functions are modeled directly in the formal model.

   function Has_Element (Self : List'Class; Position : Cursor) return Boolean
   with
     Post => Has_Element'Result = P.Mem (Positions (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Element (Self : List'Class; Position : Cursor) return Element_Type
   with
     Pre  => Has_Element (Self, Position),
     Post =>

       --  Query Positions to get the position of Position in Self and use it
       --  to fetch the corresponding element in Model.

     Element'Result = Element (Model (Self),
                               P.Get (Positions (Self), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Element);

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : List'Class) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Self) = 0 => First'Result = No_Element,
        others            => Has_Element (Self, First'Result)
        and then P.Get (Positions (Self), First'Result) = 1);

   function Next (Self : List'Class; Position : Cursor) return Cursor with
     Global         => null,
     Pre            => Has_Element (Self, Position),
     Contract_Cases =>
       (P.Get (Positions (Self), Position) = Length (Self) =>
              Next'Result = No_Element,
        others                                             =>
          Has_Element (Self, Next'Result)
        and then P.Get (Positions (Self), Next'Result) =
          P.Get (Positions (Self), Position) + 1);

   procedure Next (Self : List'Class; Position : in out Cursor) with
     Global         => null,
     Pre            => Has_Element (Self, Position),
     Contract_Cases =>
       (P.Get (Positions (Self), Position) = Length (Self) =>
              Position = No_Element,
        others                                             =>
          Has_Element (Self, Position)
        and then P.Get (Positions (Self), Position) =
          P.Get (Positions (Self), Position'Old) + 1);

   function Last (Self : List'Class) return Cursor with
     Import,
     Global         => null,
     Contract_Cases =>
       (Length (Self) = 0 => Last'Result = No_Element,
        others            => Has_Element (Self, Last'Result)
        and then P.Get (Positions (Self), Last'Result) = Length (Self));

   function Find (Self : List'Class; Element : Element_Type) return Cursor with
     Import,
     Contract_Cases =>

     --  Either Element is not in the model and the result is No_Element

     ((for all E of Self => E /= Element) => Find'Result = No_Element,

     --  or the result is a valid cursor, Element is stored at its position in
     --  Self and there is no previous occurrence of Element in L.

     others                               =>
       Has_Element (Self, Find'Result)
     and Formal_Doubly_Linked_Lists.Element (Self, Find'Result) = Element
     and (for all I in 1 .. P.Get (Positions (Self), Find'Result) - 1 =>
            Formal_Doubly_Linked_Lists.Element (Model (Self), I)
          /= Element));

   procedure Append (Self : in out List'Class; Element : Element_Type) with
     Pre            => Length (Self) < Count_Type'Last,
     Contract_Cases =>
       (Length (Self) < Capacity (Self) =>
              Capacity (Self) = Capacity (Self)'Old,
        others                          =>
          Capacity (Self) > Capacity (Self)'Old),
     Post           => Length (Self) = Length (Self)'Old + 1

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
     Pre            => Length (Self) < Count_Type'Last
     and then Has_Element (Self, Position),
     Contract_Cases =>
       (Length (Self) < Capacity (Self) =>
              Capacity (Self) = Capacity (Self)'Old,
        others                          =>
          Capacity (Self) > Capacity (Self)'Old),
     Post           => Length (Self) = Length (Self)'Old + 1

     --  Every cursor previously valid in Self is still valid.

     and (for all I of Positions (Self)'Old =>
              Has_Element (Self, I)

            --  If it was located before Position in Self its position is
          --  preserved.

          and (if P.Get (Positions (Self)'Old, I) <
                   P.Get (Positions (Self)'Old, Position)
               then P.Get (Positions (Self), I) =
                   P.Get (Positions (Self)'Old, I)

               --  Otherwise it is shifted by 1.

               else P.Get (Positions (Self), I) =
                   P.Get (Positions (Self)'Old, I) + 1))

     --  Every cursor valid in Self was previously valid except for the newly
     --  inserted cursor.

     and (for all I of Positions (Self) =>
              P.Mem (Positions (Self)'Old, I) or
            P.Get (Positions (Self), I) =
              P.Get (Positions (Self)'Old, Position))

     --  The elements of Self located before Position are preserved.

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
     Pre'Class => Has_Element (Self, Position),
     Post =>
           Element_Primitive'Result =
             Element (Model (Self), P.Get (Positions (Self), Position));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element_Primitive);

   function Has_Element_Primitive
     (Self : List; Position : Cursor) return Boolean
   with
     Post =>
       Has_Element_Primitive'Result = P.Mem (Positions (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element_Primitive);

   function Next_Primitive
     (Self : List; Position : Cursor) return Cursor
   with
     Inline,
     Pre'Class => Has_Element (Self, Position);

private
   pragma SPARK_Mode (Off);

   type List is new Element_Lists.List with null record;

end Formal_Doubly_Linked_Lists;
