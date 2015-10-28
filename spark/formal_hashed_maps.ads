pragma Ada_2012;
with Functional_Sequences;
with Functional_Maps;

generic
   type Element_Type (<>) is private;
   type Key_Type (<>) is private;
   None : Key_Type;
   with function "=" (E1, E2 : Element_Type) return Boolean;
   with function "=" (K1, K2 : Key_Type) return Boolean;
package Formal_Hashed_Maps with SPARK_Mode is

   --  To be replaced with an instance of the proper map package.

   package Element_Maps is
      type Map is tagged limited private;
      type Cursor is private;
      No_Element : constant Cursor;
   private
      pragma SPARK_Mode (Off);
      type Cursor is record
         I : Natural;
      end record;
      type Map is tagged limited null record;
      No_Element : constant Cursor := (I => 0);
   end Element_Maps;
   subtype Cursor is Element_Maps.Cursor;
   use all type Element_Maps.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Maps.No_Element;

   type Map is tagged limited private with
     Default_Initial_Condition => Length (Map) = 0;

   function Capacity (S : Map'Class) return Natural with
     Import;

   function Length (S : Map'Class) return Natural with
     Import,
     Post => Length'Result <= Capacity (S);

   package Formal_Model is
      package Cursor_Map is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor,
         No_Key       => No_Element);
      package Key_Sequence is new Functional_Sequences
        (Element_Type => Key_Type,
         "="          => "=");
      package Element_Map is new Functional_Maps
        (Element_Type => Element_Type,
         Key_Type     => Key_Type,
         No_Key       => None);
      use Key_Sequence;
      use Cursor_Map;
      use Element_Map;

      function Model (S : Map'Class) return Element_Map.Map with
        Import;

      function Keys (S : Map'Class) return Sequence with
        Import,
        Post => Length (Keys'Result) = Length (S)
        and then (for all I in 1 .. Length (S) =>
                      Mem (Model (S), Get (Keys'Result, I)))
        and then (for all E in Model (S) =>
                      (for some I in 1 .. Length (S) =>
                             Get (Keys'Result, I) = E))
        and then
            (for all I in 1 .. Length (S) =>
               (for all J in 1 .. Length (S) =>
                    (if Get (Keys'Result, I) = Get (Keys'Result, J)
                     then I = J)));

      function Positions (S : Map'Class) return Cursor_Map.Map with
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
   use Key_Sequence;
   use Cursor_Map;
   use Element_Map;

   function Key (S : Map'Class; C : Cursor) return Key_Type with
     Import,
     Pre  => Mem (Positions (S), C),
     Post => Key'Result = Get (Keys (S), Get (Positions (S), C));

   function Element (S : Map'Class; K : Key_Type) return Element_Type with
     Import,
     Pre  => Mem (Model (S), K),
     Post => Element'Result = Get (Model (S), K);

   function Element (S : Map'Class; C : Cursor) return Element_Type with
     Import,
     Pre  => Mem (Positions (S), C),
     Post => Element'Result =
       Get (Model (S), Get (Keys (S), Get (Positions (S), C)));

   function First (S : Map'Class) return Cursor with
     Import,
     Post => (if Length (S) = 0 then First'Result = No_Element
              else Mem (Positions (S), First'Result) and then
                  Get (Positions (S), First'Result) = 1);

   procedure Next (S : Map'Class; C : in out Cursor) with
     Import,
     Pre  => Mem (Positions (S), C),
     Post => (if Get (Positions (S), C'Old) = Length (S)
              then C = No_Element
              else Mem (Positions (S), C)
                and then Get (Positions (S), C) =
                  Get (Positions (S), C'Old) + 1);

   function Has_Element (S : Map'Class; C : Cursor) return Boolean with
     Import,
     Post => Has_Element'Result = Mem (Positions (S), C);

   function Contains (S : Map'Class; K : Key_Type) return Boolean with
     Import,
     Post => Contains'Result = Mem (Model (S), K);

   function Find (S : Map'Class; K : Key_Type) return Cursor with
     Import,
     Post =>
       (Find'Result = No_Element
        and not Mem (Model (S), K))
     or else
       (Mem (Model (S), K)
        and Mem (Positions (S), Find'Result)
        and Get (Keys (S), Get (Positions (S), Find'Result)) = K);

   procedure Include (S : in out Map'Class; K : Key_Type; E : Element_Type)
   with
     Import,
     Pre  => (Length (S) < Capacity (S) and then K /= None)
       or else Mem (Model (S), K),
     Post => Capacity (S) = Capacity (S)'Old
     and (if Mem (Model (S)'Old, K) then
            Length (S) = Length (S)'Old
            and Is_Replace (Model (S)'Old, K, E, Model (S))
            and Keys (S) = Keys (S)'Old
            and Positions (S) = Positions (S)'Old
          else Length (S) = Length (S)'Old + 1
            and Is_Add (Model (S)'Old, K, E, Model (S))
            and (for all C in Positions (S)'Old =>
                 Get (Keys (S), Get (Positions (S), C)) =
              Get (Keys (S)'Old, Get (Positions (S)'Old, C))));

   procedure Clear (S : in out Map'Class)
   with
       Import,
       Post => Capacity (S) = Capacity (S)'Old
     and then Length (S) = 0
     and then Is_Empty (Model (S));

private
   pragma SPARK_Mode (Off);

   type Map is new Element_Maps.Map with null record;

end Formal_Hashed_Maps;
