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
with Ada.Unchecked_Conversion;
with Interfaces;   use Interfaces;

package body Conts with SPARK_Mode is

   -------------------
   -- Ranged_Random --
   -------------------

   function Ranged_Random (Self : Random.Generator) return Random.Discrete is
      use Random;
   begin
      --  These tests are performed statically by the compiler.
      --  Special case to avoid division by zero below
      if Min = Max then
         return Min;

      elsif Min = Discrete'First
        and then Max = Discrete'Last
      then
         return Random.Random (Self);

      elsif Discrete'Base'Size > 32 then
         declare
            --  In the 64-bit case, we have to be careful, since not all 64-bit
            --  unsigned values are representable in GNAT's root_integer type.
            --  Ignore different-size warnings here since GNAT's handling
            --  is correct.

            pragma Warnings ("Z");
            function Conv_To_Unsigned is
              new Ada.Unchecked_Conversion (Discrete'Base, Unsigned_64);
            function Conv_To_Result is
              new Ada.Unchecked_Conversion (Unsigned_64, Discrete'Base);
            pragma Warnings ("z");

            N : constant Unsigned_64 :=
              Conv_To_Unsigned (Max) - Conv_To_Unsigned (Min) + 1;
            Slop : constant Unsigned_64 := Unsigned_64'Last rem N + 1;
            X    : Unsigned_64;

         begin
            loop
               X := Discrete'Pos (Random.Random (Self));
               exit when Slop = N or else X <= Unsigned_64'Last - Slop;
            end loop;
            return Conv_To_Result (Conv_To_Unsigned (Min) + X rem N);
         end;

      else
         declare
            N    : constant Unsigned_32 :=
              Unsigned_32 (Discrete'Pos (Max) - Discrete'Pos (Min) + 1);
            Slop : constant Unsigned_32 := Unsigned_32'Last rem N + 1;
            X    : Unsigned_32;
         begin
            loop
               X := Discrete'Pos (Random.Random (Self));
               exit when Slop = N or else X <= Unsigned_32'Last - Slop;
            end loop;

            return Discrete'Val
              (Discrete'Pos (Min) + Unsigned_32'Pos (X rem N));
         end;
      end if;
   end Ranged_Random;

end Conts;
