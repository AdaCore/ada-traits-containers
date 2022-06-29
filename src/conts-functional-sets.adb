--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;

package body Conts.Functional.Sets with SPARK_Mode => Off is
   use Containers;

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   ---------
   -- "=" --
   ---------

   function "=" (S1, S2 : Set) return Boolean is
     (S1.Content <= S2.Content and S2.Content <= S1.Content);

   ----------
   -- "<=" --
   ----------

   function "<=" (S1, S2 : Set) return Boolean is (S1.Content <= S2.Content);

   ---------
   -- Add --
   ---------

   function Add (S : Set; E : Element_Type) return Set is
     (Content => Add (S.Content, E));

   ------------
   -- Length --
   ------------

   function Length (S : Set) return Count_Type is (Length (S.Content));

   ---------
   -- Mem --
   ---------

   function Mem (S : Set; E : Element_Type) return Boolean is
      (Find (S.Content, E) > 0);

   ------------------
   -- Num_Overlaps --
   ------------------

   function Num_Overlaps (S1, S2 : Set) return Count_Type is
      (Num_Overlaps (S1.Content, S2.Content));

   ------------------
   -- Intersection --
   ------------------

   function Intersection (S1, S2 : Set) return Set is
     (Content => Intersection (S1.Content, S2.Content));

   ------------
   -- Is_Add --
   ------------

   function Is_Add (S : Set; E : Element_Type; Result : Set) return Boolean
   is
     (Mem (Result, E)
      and (for all F of Result => Mem (S, F) or F = E)
      and (for all E of S => Mem (Result, E)));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (S : Set) return Boolean is (Length (S.Content) = 0);

   ---------------------
   -- Is_Intersection --
   ---------------------

   function Is_Intersection (S1, S2, Result : Set) return Boolean is
     ((for all E of Result =>
            Mem (S1, E) and Mem (S2, E))
      and (for all E of S1 =>
               (if Mem (S2, E) then Mem (Result, E))));

   --------------
   -- Is_Union --
   --------------

   function Is_Union (S1, S2, Result : Set) return Boolean is
     ((for all E of Result => Mem (S1, E) or Mem (S2, E))
      and (for all E of S1 => Mem (Result, E))
      and (for all E of S2 => Mem (Result, E)));

   -----------
   -- Union --
   -----------

   function Union (S1, S2 : Set) return Set is
     (Content => Union (S1.Content, S2.Content));
end Conts.Functional.Sets;
