------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;
with Conts;            use Conts;
with Conts.Vectors.Indefinite_Unbounded;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>)  is private;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
package Functional_Maps with SPARK_Mode is

   type Map is private with
     Default_Initial_Condition => Is_Empty (Map),
     Iterable                  => (First       => Iter_First,
                                   Next        => Iter_Next,
                                   Has_Element => Iter_Has_Element,
                                   Element     => Iter_Element);
   --  Maps are empty when default initialized.
   --  For in quantification over maps should not be used.
   --  For of quantification over maps iterates over keys.

   --  Maps are axiomatized using Mem and Get encoding respectively the
   --  presence of a key in a map and an accessor to elements associated to its
   --  keys. We could also add Length.
   --  ??? Currently we do not consider potential overflows of the container
   --  implementation.

   function Mem (M : Map; K : Key_Type) return Boolean with
     Global => null;
   function Get (M : Map; K : Key_Type) return Element_Type with
     Global => null,
     Pre    => Mem (M, K);

   function Inc (M1, M2 : Map) return Boolean with
   --  Map inclusion.

     Global => null,
     Post   => Inc'Result =
       (for all K of M1 => Mem (M2, K)
        and then Get (M2, K) = Get (M1, K));

   function "=" (M1, M2 : Map) return Boolean with
   --  Extensional equality over maps.

     Global => null,
     Post   => "="'Result =
       ((for all K of M1 => Mem (M2, K)
        and then Get (M2, K) = Get (M1, K))
        and (for all K of M2 => Mem (M1, K)));

   pragma Warnings (Off, "unused variable");
   function Is_Empty (M : Map) return Boolean with
   --  A map is empty if it contains no key.

     Global => null,
     Post   => Is_Empty'Result = (for all K of M => False);
   pragma Warnings (On, "unused variable");

   function Is_Add
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   --  Returns True if Result is M augmented with the mapping K -> E.

   with
     Global => null,
     Post   => Is_Add'Result =
         (not Mem (M, K)
          and then Mem (Result, K) and then Get (Result, K) = E
          and then (for all K of M => Mem (Result, K)
                    and then Get (Result, K) = Get (M, K))
          and then (for all KK of Result => KK = K or Mem (M, KK)));

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map with
   --  Returns M augmented with the mapping K -> E.
   --  Is_Add (M, K, E, Result) should be used instead of
   --  Result = Add (M, K, E) whenever possible both for execution and for
   --  proof.

     Global => null,
     Pre    => not Mem (M, K),
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
          and then (for all KK of M => Mem (Result, KK)
                    and then
                      (if K /= KK
                         then Get (Result, KK) = Get (M, KK)))
          and then (for all K of Result => Mem (M, K)));

   function Replace (M : Map; K : Key_Type; E : Element_Type) return Map with
   --  Returns M where the element associated to K has been replaced by E.
   --  Is_Replace (M, K, E, Result) should be instead of than
   --  Result = Replace (M, K, E) whenever possible both for execution and for
   --  proof.

     Global => null,
     Pre    => Mem (M, K),
     Post   => Is_Replace (M, K, E, Replace'Result);

   --  For quantification purpose
   --  Do not use them in practice
   --  ??? Is there a way to prevent users from using those ?
   type Private_Key is private;

   function Iter_First (M : Map) return Private_Key with
     Global => null;
   function Iter_Has_Element (M : Map; K : Private_Key) return Boolean with
     Global => null;
   function Iter_Next (M : Map; K : Private_Key) return Private_Key with
     Global => null,
     Pre    => Iter_Has_Element (M, K);
   function Iter_Element (M : Map; K : Private_Key) return Key_Type with
     Global => null,
     Pre    => Iter_Has_Element (M, K);
   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", Mem);
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
      Index_Type          => Count_Type,
      Container_Base_Type => Neither_Controlled_Nor_Limited);

   package Key_Lists is new Conts.Vectors.Indefinite_Unbounded
     (Element_Type        => Key_Type,
      Index_Type          => Count_Type,
      Container_Base_Type => Neither_Controlled_Nor_Limited);

   type Map is record
      Keys     : Key_Lists.Vector;
      Elements : Element_Lists.Vector;
   end record;

   type Private_Key is new Key_Lists.Cursor;
end Functional_Maps;
