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

   ----------------------
   -- Count_If_Convert --
   ----------------------

   function Count_If_Convert
      (Self      : Cursors.Cursors.Container;
       Predicate : not null access function
          (E : Cursors.Element) return Boolean)
      return Natural
   is
      C : Cursors.Cursors.Cursor := Cursors.Cursors.First (Self);
      Count : Natural := 0;
   begin
      while Cursors.Cursors.Has_Element (Self, C) loop
         if Predicate
            (Cursors.Convert (Cursors.Cursors.Element (Self, C)))
         then
            Count := Count + 1;
         end if;
         C := Cursors.Cursors.Next (Self, C);
      end loop;
      return Count;
   end Count_If_Convert;

   --------------
   -- Count_If --
   --------------

   function Count_If
      (Self      : Cursors.Container;
       Predicate : not null access function
          (E : Cursors.Returned) return Boolean)
      return Natural
   is
      function Identity (E : Cursors.Returned) return Cursors.Returned
         is (E) with Inline;
      package C is new Conts.Cursors.Constant_Forward_Convert_Traits
         (Cursors, Cursors.Returned, Identity);
      function Internal is new Count_If_Convert (C);
   begin
      return Internal (Self, Predicate);
   end Count_If;

   -------------
   -- Shuffle --
   -------------

   procedure Shuffle
     (Self : in out Cursors.Container;
      Gen  : Random.Generator)
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
            function Rand is new Conts.Ranged_Random
              (Random, First, C);
         begin
            G := Rand (Gen);
            Swap (Self, G, C);
            C := C - 1;
         end;
      end loop;
   end Shuffle;

end Conts.Algorithms;
