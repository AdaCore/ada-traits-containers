pragma Ada_2012;
with Functional_Sequences;
with Functional_Maps;
with Functional_Sets;

generic
   type Element_Type (<>) is private;
   None : Element_Type;
   --  ASSUMPTION: "=" and "<" should be compatible.
   with function "<" (E1, E2 : Element_Type) return Boolean;
   with function "=" (E1, E2 : Element_Type) return Boolean;
package Formal_Ordered_Sets with SPARK_Mode is

   --  To be replaced with an instance of the proper set package.

   package Element_Sets is
      type Set is tagged limited private;
      type Cursor is private;
      No_Element : constant Cursor;
   private
      pragma SPARK_Mode (Off);
      type Cursor is record
         I : Natural;
      end record;
      type Set is tagged limited null record;
      No_Element : constant Cursor := (I => 0);
   end Element_Sets;
   subtype Cursor is Element_Sets.Cursor;
   use all type Element_Sets.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Sets.No_Element;

   type Set is tagged limited private with
     Default_Initial_Condition => Length (Set) = 0;

   function Capacity (S : Set'Class) return Natural with
     Import;

   function Length (S : Set'Class) return Natural with
     Import,
     Post => Length'Result <= Capacity (S);

   package Formal_Model is
      package Cursor_Map is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor,
         No_Key       => No_Element);
      package Element_Sequence is new Functional_Sequences
        (Element_Type => Element_Type,
         "="          => "=");
      package Element_Set is new Functional_Sets
        (Element_Type => Element_Type,
         No_Element   => None,
         "="          => "=");
      use Element_Sequence;
      use Cursor_Map;
      use Element_Set;

      function Model (S : Set'Class) return Element_Set.Set with
        Import;

      function Elements (S : Set'Class) return Sequence with
        Import,
        Post => Length (Elements'Result) = Length (S)
        and then (for all I in 1 .. Length (S) =>
                      Mem (Model (S), Get (Elements'Result, I)))
        and then (for all E in Model (S) =>
                      (for some I in 1 .. Length (S) =>
                             Get (Elements'Result, I) = E))
        and then
            (for all I in 1 .. Length (S) =>
               (for all J in 1 .. Length (S) =>
                    (Get (Elements'Result, I) < Get (Elements'Result, J))
                      = (I < J)));

      function Positions (S : Set'Class) return Map with
        Import,
        Post =>
          (for all C1 in Positions'Result =>
             Get (Positions'Result, C1) in 1 .. Length (S)
           and then
             (for all C2 in Positions'Result =>
                (if Get (Positions'Result, C1) =
                     Get (Positions'Result, C2)
                 then C1 = C2)));
   end Formal_Model;
   use Formal_Model;
   use Element_Sequence;
   use Cursor_Map;
   use Element_Set;

   function Element (S : Set'Class; C : Cursor) return Element_Type with
     Import,
     Pre  => Mem (Positions (S), C),
     Post => Element'Result = Get (Elements (S), Get (Positions (S), C));

   function First (S : Set'Class) return Cursor with
     Import,
     Post => (if Length (S) = 0 then First'Result = No_Element
              else Mem (Positions (S), First'Result) and then
                  Get (Positions (S), First'Result) = 1);

   procedure Next (S : Set'Class; C : in out Cursor) with
     Import,
     Pre  => Mem (Positions (S), C),
     Post => (if Get (Positions (S), C'Old) = Length (S)
              then C = No_Element
              else Mem (Positions (S), C)
                and then Get (Positions (S), C) =
                  Get (Positions (S), C'Old) + 1);

   function Has_Element (S : Set'Class; C : Cursor) return Boolean with
     Import,
     Post => Has_Element'Result = Mem (Positions (S), C);

   function Contains (S : Set'Class; E : Element_Type) return Boolean with
     Import,
     Post => Contains'Result = Mem (Model (S), E);

   function Find (S : Set'Class; E : Element_Type) return Cursor with
     Import,
     Post =>
       (Find'Result = No_Element
        and not Mem (Model (S), E))
     or else
       (Mem (Model (S), E)
        and Mem (Positions (S), Find'Result)
        and Get (Elements (S), Get (Positions (S), Find'Result)) = E);

   procedure Include (S : in out Set'Class; E : Element_Type) with
     Import,
     Pre  => (Length (S) < Capacity (S) and then E /= None)
     or else Mem (Model (S), E),
     Post => Capacity (S) = Capacity (S)'Old
     and (if Mem (Model (S)'Old, E) then
            Length (S) = Length (S)'Old
            and Model (S) = Model (S)'Old
            and Elements (S) = Elements (S)'Old
            and Positions (S) = Positions (S)'Old
          else Length (S) = Length (S)'Old + 1
            and Is_Add (Model (S)'Old, E, Model (S))
            and (for all I in 1 .. Length (S) - 1 =>
                  (if I < Get (Positions (S), Find (S, E))
                   then Get (Elements (S)'Old, I) = Get (Elements (S), I)
                   else Get (Elements (S)'Old, I) = Get (Elements (S), I + 1)))
            and (for all C in Positions (S)'Old =>
                     (if Get (Elements (S), Get (Positions (S), C)) < E
                     then Get (Positions (S), C) =
                        Get (Positions (S)'Old, C)
                     else Get (Positions (S), C) =
                        Get (Positions (S)'Old, C) + 1))
            and (for all C in Positions (S)'Old =>
                   Mem (Positions (S), C) and
                 Get (Elements (S), Get (Positions (S), C)) =
                Get (Elements (S)'Old, Get (Positions (S)'Old, C))));

   procedure Exclude (S : in out Set'Class; E : Element_Type) with
     Import,
     Post => Capacity (S) = Capacity (S)'Old
     and (if not Mem (Model (S)'Old, E) then
            Length (S) = Length (S)'Old
            and Model (S) = Model (S)'Old
            and Elements (S) = Elements (S)'Old
            and Positions (S) = Positions (S)'Old
          else Length (S) = Length (S)'Old - 1
            and Is_Add (Model (S), E, Model (S)'Old)
            and (for all I in 1 .. Length (S) =>
                  (if I < Get (Positions (S)'Old, Find (S, E)'Old)
                   then Get (Elements (S), I) = Get (Elements (S)'Old, I)
                   else Get (Elements (S), I) = Get (Elements (S)'Old, I + 1)))
            and (for all C in Positions (S) =>
                     (if Get (Elements (S), Get (Positions (S), C)) < E
                     then Get (Positions (S)'Old, C) =
                        Get (Positions (S), C)
                     else Get (Positions (S)'Old, C) =
                        Get (Positions (S), C) + 1))
            and (for all C in Positions (S) =>
                   Mem (Positions (S)'Old, C) and
                 Get (Elements (S), Get (Positions (S), C)) =
                 Get (Elements (S)'Old, Get (Positions (S)'Old, C))));

   procedure Union (S1 : in out Set'Class; S2 : Set'Class) with
     Import,
     Pre  => Length (S2) <= Capacity (S1) - Length (S1),
     Post => Capacity (S1) = Capacity (S1)'Old
     and Is_Union (Model (S1)'Old, Model (S2), Model (S1))
     and Length (S1) in Length (S1)'Old .. Length (S1)'Old + Length (S2)
     and (for all C in Positions (S1)'Old =>
              Get (Elements (S1), Get (Positions (S1), C)) =
            Get (Elements (S1)'Old, Get (Positions (S1)'Old, C)));

   procedure Intersection (S1 : in out Set'Class; S2 : Set'Class) with
     Import,
     Post => Capacity (S1) = Capacity (S1)'Old
     and Is_Intersection (Model (S1)'Old, Model (S2), Model (S1))
     and Length (S1) in 0 .. Length (S1)'Old;

   procedure Clear (S : in out Set'Class)
   with
       Import,
       Post => Capacity (S) = Capacity (S)'Old
     and then Length (S) = 0
     and then Is_Empty (Model (S));

private
   pragma SPARK_Mode (Off);

   type Set is new Element_Sets.Set with null record;

end Formal_Ordered_Sets;
