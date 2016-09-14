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
      if L_Last - L_First /= R_Last - R_First then
         return False;
      end if;

      for L in L_First .. L_Last loop
         if Getters.Get (Left, L) /=
            Getters.Get (Right, R_First + (L - L_First))
         then
            return False;
         end if;
      end loop;

      return True;
   end Equals;

end Conts.Algorithms;
