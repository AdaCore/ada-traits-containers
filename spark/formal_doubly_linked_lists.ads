pragma Ada_2012;
with Conts.Lists.Indefinite_Unbounded_SPARK;
with Functional_Sequences;
with Functional_Maps;

generic
   type Element_Type is private;
   with function "=" (E1, E2 : Element_Type) return Boolean;
package Formal_Doubly_Linked_Lists with SPARK_Mode is
   package Element_Lists is new Conts.Lists.Indefinite_Unbounded_SPARK
     (Element_Type => Element_Type);
   subtype Cursor is Element_Lists.Cursor;
   use all type Element_Lists.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Lists.Lists.No_Element;

   type List is tagged limited private with
     Default_Initial_Condition => Length (List) = 0;

   function Capacity (L : List'Class) return Natural;

   function Length (L : List'Class) return Natural with
     Post => Length'Result <= Capacity (L);

   package Model is
      package Cursor_Map is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor,
         No_Key       => No_Element,
         "="          => "=");
      package Element_Sequence is new Functional_Sequences
        (Element_Type => Element_Type,
         "="          => "=");
      use Element_Sequence;
      use Cursor_Map;

      function Get_Model (L : List'Class) return Sequence with
        Post => Length (Get_Model'Result) = Length (L);

      function Get_Positions (L : List'Class) return Map with
        Post =>
          (for all C1 in Get_Positions'Result =>
             Get (Get_Positions'Result, C1) in 1 .. Length (L)
           and then
             (for all C2 in Get_Positions'Result =>
                (if Get (Get_Positions'Result, C1) =
                     Get (Get_Positions'Result, C2)
                 then C1 = C2)));
   end Model;
   use Model;
   use Element_Sequence;
   use Cursor_Map;

   function Element (L : List'Class; C : Cursor) return Element_Type with
     Pre  => Mem (Get_Positions (L), C),
     Post => Element'Result =
       Get (Get_Model (L), Get (Get_Positions (L), C));

   function First (L : List'Class) return Cursor with
     Post => (if Length (L) = 0 then First'Result = No_Element
              else Mem (Get_Positions (L), First'Result) and then
                  Get (Get_Positions (L), First'Result) = 1);

   procedure Next (L : List'Class; C : in out Cursor) with
     Pre  => Mem (Get_Positions (L), C),
     Post => (if Get (Get_Positions (L), C'Old) = Length (L)
              then C = No_Element
              else Mem (Get_Positions (L), C)
                and then Get (Get_Positions (L), C) =
                  Get (Get_Positions (L), C'Old) + 1);

   function Has_Element (L : List'Class; C : Cursor) return Boolean with
     Post => Has_Element'Result = Mem (Get_Positions (L), C);

   function Find (L : List'Class; E : Element_Type) return Cursor with
     Import,
     Post =>
       (Find'Result = No_Element
        and (for all I in 1 .. Length (L) => Get (Get_Model (L), I) /= E))
     or else
       (Mem (Get_Positions (L), Find'Result)
        and Get (Get_Model (L), Get (Get_Positions (L), Find'Result)) = E
        and (for all I in 1 .. Get (Get_Positions (L), Find'Result) - 1 =>
               Get (Get_Model (L), I) /= E));

   procedure Append (L : in out List'Class; E : Element_Type) with
     Pre  => Length (L) < Capacity (L),
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old + 1
     and Inc (Get_Positions (L)'Old, Get_Positions (L))
     and (for all C in Get_Positions (L) =>
              Mem (Get_Positions (L)'Old, C) or
              Get (Get_Positions (L), C) = Length (L))
     and (for all I in 1 .. Length (L) - 1 =>
              Get (Get_Model (L), I) =
              Get (Get_Model (L)'Old, I))
     and Get (Get_Model (L), Length (L)) = E;

   procedure Insert (L : in out List'Class; C : Cursor; E : Element_Type) with
     Import,
     Pre  => Length (L) < Capacity (L) and then Has_Element (L, C),
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old + 1
     and (for all D in Get_Positions (L)'Old =>
              Mem (Get_Positions (L), D) and
              (if Get (Get_Positions (L)'Old, D) <
                   Get (Get_Positions (L)'Old, C)
               then Get (Get_Positions (L), D) = Get (Get_Positions (L)'Old, D)
               else Get (Get_Positions (L), D) =
                 Get (Get_Positions (L)'Old, D) + 1))
     and (for all D in Get_Positions (L) =>
              Mem (Get_Positions (L)'Old, D) or
              Get (Get_Positions (L), D) = Get (Get_Positions (L)'Old, C))
     and (for all I in 1 .. Get (Get_Positions (L)'Old, C) - 1 =>
              Get (Get_Model (L), I) =
              Get (Get_Model (L)'Old, I))
     and (for all I in Get (Get_Positions (L)'Old, C) + 1 .. Length (L) =>
              Get (Get_Model (L), I) =
              Get (Get_Model (L)'Old, I - 1))
     and Get (Get_Model (L), Get (Get_Positions (L)'Old, C)) = E;

   procedure Replace_Element
     (L : in out List'Class; C : Cursor; E : Element_Type) with
     Import,
     Pre  => Has_Element (L, C),
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old
     and Inc (Get_Positions (L)'Old, Get_Positions (L))
     and (for all D in Get_Positions (L) =>
              Mem (Get_Positions (L)'Old, D))
     and (for all I in 1 .. Length (L) =>
              (if I /= Get (Get_Positions (L)'Old, C) then
                   Get (Get_Model (L), I) = Get (Get_Model (L)'Old, I)))
     and Get (Get_Model (L), Get (Get_Positions (L)'Old, C)) = E;

   procedure Clear (L : in out List'Class)
   with
       Post => Capacity (L) = Capacity (L)'Old
     and then Length (L) = 0;

private
   pragma SPARK_Mode (Off);

   type List is new Element_Lists.List with null record;

end Formal_Doubly_Linked_Lists;
