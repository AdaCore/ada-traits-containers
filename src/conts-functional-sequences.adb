--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
package body Conts.Functional.Sequences with SPARK_Mode => Off is
   use Containers;

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   ---------
   -- "=" --
   ---------

   function "=" (S1, S2 : Sequence) return Boolean is
     (S1.Content = S2.Content);

   ---------
   -- Add --
   ---------

   function Add (S : Sequence; E : Element_Type) return Sequence is
     (Content => Add (S.Content, E));

   ---------
   -- Get --
   ---------

   function Get (S : Sequence; N : Extended_Index) return Element_Type is
     (Get (S.Content, N));

   ------------
   -- Is_Add --
   ------------

   function Is_Add
     (S : Sequence; E : Element_Type; Result : Sequence) return Boolean is
     (Length (Result) = Length (S) + 1
      and then Get (Result, Index_Type'Val
                    ((Index_Type'Pos (Index_Type'First) - 1) +
                       Length (Result))) = E
      and then
        (for all M in Index_Type'First ..
             (Index_Type'Val
                  ((Index_Type'Pos (Index_Type'First) - 1) + Length (S))) =>
              Get (Result, M) = Get (S, M)));

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (S : Sequence; N : Index_Type; E : Element_Type; Result : Sequence)
      return Boolean is
     (N in Index_Type'First ..
             (Index_Type'Val
                  ((Index_Type'Pos (Index_Type'First) - 1) + Length (S)))
      and then Length (Result) = Length (S)
      and then Get (Result, N) = E
      and then
        (for all M in  Index_Type'First ..
             (Index_Type'Val
                  ((Index_Type'Pos (Index_Type'First) - 1) + Length (S))) =>
             (if M /= N then Get (Result, M) = Get (S, M))));

   ----------
   -- Last --
   ----------

   function Last (S : Sequence) return Extended_Index is
     (Index_Type'Val ((Index_Type'Pos (Index_Type'First) - 1) + Length (S)));

   ------------
   -- Length --
   ------------

   function Length (S : Sequence) return Count_Type is
     (Length (S.Content));

   ---------
   -- Set --
   ---------

   function Set (S : Sequence; N : Index_Type; E : Element_Type)
                 return Sequence is
     (Content => Set (S.Content, N, E));
end Conts.Functional.Sequences;
