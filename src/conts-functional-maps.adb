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
package body Conts.Functional.Maps with SPARK_Mode => Off is
   use Key_Containers;
   use Element_Containers;

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   ---------
   -- "=" --
   ---------

   function "=" (M1, M2 : Map) return Boolean is
     (M1.Keys <= M2.Keys and M2 <= M1);

   ----------
   -- "<=" --
   ----------

   function "<=" (M1, M2 : Map) return Boolean is
      I2 : Count_Type;
   begin
      for I1 in 1 .. Length (M1.Keys) loop
         I2 := Find (M2.Keys, Get (M1.Keys, I1));
         if I2 = 0
           or else Get (M2.Elements, I2) /= Get (M1.Elements, I1)
         then
            return False;
         end if;
      end loop;
      return True;
   end "<=";

   ---------
   -- Add --
   ---------

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map is
     (Keys     => Add (M.Keys, K),
      Elements => Add (M.Elements, E));

   ---------
   -- Get --
   ---------

   function Get (M : Map; K : Key_Type) return Element_Type is
     (Get (M.Elements, Find (M.Keys, K)));

   ------------
   -- Is_Add --
   ------------

   function Is_Add
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   is
     (not Mem (M, K)
      and then Mem (Result, K) and then Get (Result, K) = E
      and then (for all K of M => Mem (Result, K)
                and then Get (Result, K) = Get (M, K))
      and then (for all KK of Result => KK = K or Mem (M, KK)));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (M : Map) return Boolean is
     (Length (M.Keys) = 0);

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   is
     (Mem (M, K)
      and then Mem (Result, K)
      and then Get (Result, K) = E
      and then (for all KK of M => Mem (Result, KK)
                and then
                  (if K /= KK
                   then Get (Result, KK) = Get (M, KK)))
      and then (for all K of Result => Mem (M, K)));

   ------------
   -- Length --
   ------------

   function Length (M : Map) return Count_Type is (Length (M.Elements));

   ---------
   -- Mem --
   ---------

   function Mem (M : Map; K : Key_Type) return Boolean is
     (Find (M.Keys, K) > 0);

   ---------
   -- Set --
   ---------

   function Set (M : Map; K : Key_Type; E : Element_Type) return Map is
     (Keys => M.Keys, Elements => Set (M.Elements, Find (M.Keys, K), E));
end Conts.Functional.Maps;
