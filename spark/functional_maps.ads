pragma Ada_2012;
with Conts.Vectors.Indefinite_Unbounded;

generic
   type Key_Type (<>) is private;
   No_Key : Key_Type;
   --  Special key which cannot be contained in any map. This is needed to
   --  use the Iterable aspect without introducing indirections (which would
   --  be bad for proof).
   --  ??? Can we imagine a way to remove it ?

   type Element_Type (<>)  is private;
package Functional_Maps with SPARK_Mode is

   type Map is private with
     Default_Initial_Condition => Is_Empty (Map),
     Iterable                  => (First       => First_Key,
                                   Next        => Next_Key,
                                   Has_Element => Mem,
                                   Element     => Get);
   --  Maps are empty when default initialized.
   --  For in quantification over maps iterates over keys.
   --  For of quantification over maps iterates over elements.

   --  Maps are axiomatized using Mem and Get encoding respectively the
   --  presence of a key in a map and an accessor to elements associated to its
   --  keys. We could also add Length.
   --  ??? Currently we do not consider potential overflows of the container
   --  implementation.

   function Mem (M : Map; K : Key_Type) return Boolean with
     Global => null,
     Post   => (if K = No_Key then not Mem'Result);
   function Get (M : Map; K : Key_Type) return Element_Type with
     Global => null,
     Pre    => Mem (M, K);

   function Inc (M1, M2 : Map) return Boolean with
   --  Map inclusion.

     Global => null,
     Post   => Inc'Result =
       (for all K in M1 => Mem (M2, K)
        and then Get (M2, K) = Get (M1, K));

   function "=" (M1, M2 : Map) return Boolean with
   --  Extensional equality over maps.

     Global => null,
     Post   => "="'Result =
       ((for all K in M1 => Mem (M2, K)
        and then Get (M2, K) = Get (M1, K))
        and (for all K in M2 => Mem (M1, K)));

   pragma Warnings (Off, "unused variable");
   function Is_Empty (M : Map) return Boolean with
   --  A map is empty if it contains no key.

     Global => null,
     Post   => Is_Empty'Result = (for all K in M => False);
   pragma Warnings (On, "unused variable");

   function Is_Add
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   --  Returns True if Result is M augmented with the mapping K -> E.

   with
     Global => null,
     Post   => Is_Add'Result =
         (K /= No_Key and then not Mem (M, K)
          and then Mem (Result, K) and then Get (Result, K) = E
          and then (for all K in M => Mem (Result, K)
                    and then Get (Result, K) = Get (M, K))
          and then (for all KK in Result => KK = K or Mem (M, KK)));

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map with
   --  Returns M augmented with the mapping K -> E.
   --  Is_Add (M, K, E, Result) should be used instead of
   --  Result = Add (M, K, E) whenever possible both for execution and for
   --  proof.

     Global => null,
     Pre    => K /= No_Key and then not Mem (M, K),
     Post   => Is_Add (M, K, E, Add'Result);

   function Is_Replace
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   --  Returns True if Result is M where the element associated to K has been
   --  replaced by E.

   with
     Global => null,
     Post   => Is_Replace'Result =
         (Mem (M, K)
          and then Mem (Result, K)
          and then Get (Result, K) = E
          and then (for all KK in M => Mem (Result, KK)
                    and then
                      (if K /= KK
                         then Get (Result, KK) = Get (M, KK)))
          and then (for all K in Result => Mem (M, K)));

   function Replace (M : Map; K : Key_Type; E : Element_Type) return Map with
   --  Returns M where the element associated to K has been replaced by E.
   --  Is_Replace (M, K, E, Result) should be instead of than
   --  Result = Replace (M, K, E) whenever possible both for execution and for
   --  proof.

     Global => null,
     Pre    => Mem (M, K),
     Post   => Is_Replace (M, K, E, Replace'Result);

   --  For quantification purpose
   --  ??? Those are really inefficient. Do we want to do something about it ?
   function First_Key (M : Map) return Key_Type with
     Global => null;
   function Next_Key (M : Map; K : Key_Type) return Key_Type with
     Global => null,
     Pre    => Mem (M, K);
private
   pragma SPARK_Mode (Off);

   type Neither_Controlled_Nor_Limited is tagged null record;

   --  Functional maps are neither controlled nor limited. As a result,
   --  no primitive should be provided to modify them. Note that we
   --  should definitely not use limited types for those as we need to apply
   --  'Old on them.
   --  ??? Should we make them controlled to avoid memory leak ?

   package Element_Lists is new Conts.Vectors.Indefinite_Unbounded
     (Element_Type        => Element_Type,
      Index_Type          => Positive,
      Container_Base_Type => Neither_Controlled_Nor_Limited);

   package Key_Lists is new Conts.Vectors.Indefinite_Unbounded
     (Element_Type        => Key_Type,
      Index_Type          => Positive,
      Container_Base_Type => Neither_Controlled_Nor_Limited);

   type Map is record
      Keys     : Key_Lists.Vector;
      Elements : Element_Lists.Vector;
   end record;
end Functional_Maps;
