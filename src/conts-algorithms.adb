------------------------------------------------------------------------------
--                     Copyright (C) 2015-2016, AdaCore                     --
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

package body Conts.Algorithms is

   --------------------------------
   -- Count_If_With_External_Get --
   --------------------------------

   function Count_If_With_External_Get
      (Self      : Cursors.Container;
       Map       : Getters.Map;
       Predicate : not null access
         function (E : Getters.Element) return Boolean)
      return Natural
   is
      C     : Cursors.Cursor := Cursors.First (Self);
      Count : Natural := 0;
   begin
      while Cursors.Has_Element (Self, C) loop
         if Predicate (Getters.Get (Map, C)) then
            Count := Count + 1;
         end if;
         C := Cursors.Next (Self, C);
      end loop;
      return Count;
   end Count_If_With_External_Get;

   --------------
   -- Count_If --
   --------------

   function Count_If
     (Self      : Cursors.Container;
      Predicate : not null access function
        (E : Getters.Element) return Boolean)
       return Natural
   is
      function Internal is new Count_If_With_External_Get (Cursors, Getters);
   begin
      return Internal (Self, Self, Predicate);
   end Count_If;

   -------------
   -- Shuffle --
   -------------

   procedure Shuffle
     (Self : in out Cursors.Container;
      Gen  : in out Random.Generator)
   is
      use Cursors;
      First         : constant Cursors.Index := Cursors.First (Self);
      Next_To_First : constant Cursors.Index := First + 1;
      Last  : constant Cursors.Index := Cursors.Last (Self);
      C     : Cursors.Index := Last;
      G     : Cursors.Index;
   begin
      --  Fisher and Yates algorithm
      --  http://en.wikipedia.org/wiki/Fisher-Yates_shuffle

      while C /= Next_To_First loop
         declare
            --  The cost of the instance is limited (just a few instructions)
            --  thanks to inlining.
            procedure Rand is new Conts.Ranged_Random (Random, First, C);
         begin
            Rand (Gen, Result => G);
            Swap (Self, G, C);
            C := C - 1;
         end;
      end loop;
   end Shuffle;

   --------------------
   -- Insertion_Sort --
   --------------------

   procedure Insertion_Sort (Self : in out Cursors.Container) is
      C  : Cursors.Index := Cursors.First (Self);
   begin
      if Cursors.Has_Element (Self, C) then
         C := Cursors.Next (Self, C);
         while Cursors.Has_Element (Self, C) loop
            declare
               Elem : constant Getters.Element := Getters.Get (Self, C);
               D, E : Cursors.Index;
            begin
               E := C;
               loop
                  D := Cursors.Previous (Self, E);
                  exit when not Cursors.Has_Element (Self, D)
                    or else "<" (Getters.Get (Self, D), Elem);
                  Swap (Self, E, D);
                  E := D;
               end loop;
            end;

            C := Cursors.Next (Self, C);
         end loop;
      end if;
   end Insertion_Sort;

   ----------------
   -- Shell_Sort --
   ----------------

   procedure Shell_Sort (Self : in out Cursors.Container) is
      --  See https://en.wikipedia.org/wiki/Shellsort

      First : constant Cursors.Index := Cursors.First (Self);
      C     : Cursors.Index;
   begin
      for Gap of reverse Gaps loop
         --  Do a gapped insertion sort for this gap size.
         --  The first gap elements are already in gap order.

         C := Cursors.Add (First, Gap);
         while Cursors.Has_Element (Self, C) loop
            declare
               Elem : constant Getters.Element := Getters.Get (Self, C);
               D, E : Cursors.Index;
            begin
               E := C;
               loop
                  D := Cursors."+" (E, -Gap);
                  exit when "<" (Getters.Get (Self, D), Elem);
                  Swap (Self, E, D);
                  exit when Cursors.Distance (D, First) < Gap;
                  E := D;
               end loop;
            end;

            C := Cursors.Next (Self, C);
         end loop;
      end loop;
   end Shell_Sort;

   ----------------------------------
   -- Ranged_Random_Access_Cursors --
   ----------------------------------

   package body Ranged_Random_Access_Cursors is

      ----------
      -- Swap --
      ----------

      procedure Swap (Self : in out Rg; Left, Right :  Cursors.Index) is
      begin
         Base_Swap (Self.Base.all, Left, Right);
      end Swap;

   end Ranged_Random_Access_Cursors;

   ---------------
   -- Quicksort --
   ---------------

   procedure Quicksort (Self : in out Cursors.Container) is
      package Ranges is new Ranged_Random_Access_Cursors
        (Cursors, Getters, Swap);
      procedure Shell is new Shell_Sort
        (Ranges.Cursors, Ranges.Getters, "<", Ranges.Swap);

      procedure Recurse (Low, High : Cursors.Index);
      procedure Recurse (Low, High : Cursors.Index) is
         Dist : Integer;
         L    : Cursors.Index := Low;
         H    : Cursors.Index := High;
         Left, Right : Cursors.Index;
      begin
         loop
            Dist := Cursors.Dist (H, L);
            exit when Dist <= 0;

            if Dist < Threshold then
               declare
                  S : Ranges.Rg := Ranges.Subset
                    (Self'Unrestricted_Access, L, H);
               begin
                  Shell (S);
               end;
               return;
            end if;

            Left := L;
            Right := H;

            declare
               Pivot : constant Getters.Element :=
                 Getters.Value (Self, Cursors.Add (L, Dist / 2));
            begin
               --  ??? Should handle cases where the element is equal to the
               --  pivot, to avoid the worst case where the sequences contains
               --  only equal items.

               loop
                  while "<" (Getters.Value (Self, Left), Pivot) loop
                     Left := Cursors.Next (Self, Left);
                  end loop;

                  while "<" (Pivot, Getters.Value (Self, Right)) loop
                     Right := Cursors.Previous (Self, Right);
                  end loop;

                  exit when Cursors.Dist (Right, Left) <= 0;

                  Swap (Self, Right, Left);
                  Left := Cursors.Next (Self, Left);
                  Right := Cursors.Previous (Self, Right);
               end loop;
            end;

            --  Recurse for smaller sequence, and tail recursion for longer
            --  one. Do not keep pivot on the stack while recursing.
            if Cursors.Dist (Right, L) > Cursors.Dist (H, Right) then
               Recurse (Cursors.Next (Self, Right), H);
               H := Right;  --  loop on L..Right
            else
               Recurse (L, Right);
               L := Cursors.Next (Self, Right);  --  loop on Right+1 .. H
            end if;
         end loop;
      end Recurse;

   begin
      Recurse (Cursors.First_Index (Self), Cursors.Last_Index (Self));
   end Quicksort;

   ---------------
   -- Is_Sorted --
   ---------------

   function Is_Sorted (Self : Cursors.Container) return Boolean is
      Prev  : Cursors.Cursor := Cursors.First (Self);
      C     : Cursors.Cursor;
   begin
      if not Cursors.Has_Element (Self, Prev) then
         return True;  --  an empty sequence is always sorted
      end if;

      C := Cursors.Next (Self, Prev);

      while Cursors.Has_Element (Self, C) loop
         if Getters.Get (Self, C) < Getters.Get (Self, Prev) then
            return False;
         end if;
         Prev := C;
         C := Cursors.Next (Self, C);
      end loop;
      return True;
   end Is_Sorted;

   ----------
   -- Find --
   ----------

   function Find
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Cursors.Cursor
   is
      C     : Cursors.Cursor := Cursors.First (Self);
   begin
      while Cursors.Has_Element (Self, C) loop
         if Getters.Get (Self, C) = E then
            return C;
         end if;
         C := Cursors.Next (Self, C);
      end loop;
      return Cursors.No_Element;
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Boolean
   is
      function F is new Find (Cursors, Getters, "=");
      use type Cursors.Cursor_Type;
   begin
      return F (Self, E) /= Cursors.No_Element;
   end Contains;

   ------------
   -- Equals --
   ------------

   function Equals (Left, Right  : Cursors.Container) return Boolean is
      use Cursors;
      L_First : constant Cursors.Index_Type := Cursors.First (Left);
      L_Last  : constant Cursors.Index_Type := Cursors.Last (Left);
      R_First : constant Cursors.Index_Type := Cursors.First (Right);
      R_Last  : constant Cursors.Index_Type := Cursors.Last (Right);
   begin
      if Distance (L_Last, L_First) /= Distance (R_Last, R_First) then
         return False;
      end if;

      for L in L_First .. L_Last loop
         if Getters.Get (Left, L) /=
            Getters.Get (Right, R_First + Distance (L, L_First))
         then
            return False;
         end if;
      end loop;

      return True;
   end Equals;

end Conts.Algorithms;
