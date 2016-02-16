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
with Conts.Algorithms;
with Conts.Adaptors;     use Conts.Adaptors;

package body Perf_Support is

   -----------
   -- Image --
   -----------

   function Image (P : Integer) return String is
      Img : constant String := P'Img;
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Image;

   ------------
   -- Assert --
   ------------

   procedure Assert (Count, Expected : Natural; Reason : String := "") is
   begin
      if Count /= Expected then
         raise Program_Error with "Wrong count (" & Reason & "): got"
            & Count'Img & " expected" & Expected'Img;
      end if;
   end Assert;

   ---------------------
   -- Test_Arrays_Int --
   ---------------------

   procedure Test_Arrays_Int (Stdout : not null access Output'Class) is
      type Int_Array is array (Integer range <>) of Integer;
      package Adaptors is new Array_Adaptors
         (Index_Type   => Integer,
          Element_Type => Integer,
          Array_Type   => Int_Array);
      function Count_If is new Conts.Algorithms.Count_If
         (Adaptors.Cursors.Forward, Adaptors.Maps.Element);

      procedure Run (V : in out Int_Array);
      procedure Run (V : in out Int_Array) is
         Co : Natural := 0;
      begin
         Stdout.Start_Test ("fill");
         for C in 1 .. Items_Count loop
            V (C) := 2;
         end loop;
         Stdout.End_Test;

         Stdout.Start_Test ("copy");
         declare
            V_Copy : Int_Array := V;
            pragma Unreferenced (V_Copy);
         begin
            Stdout.End_Test;
         end;

         Co := 0;
         Stdout.Start_Test ("cursor loop");
         for It in V'Range loop
            if Predicate (V (It)) then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.End_Test;
         Assert (Co, Items_Count);

         Co := 0;
         Stdout.Start_Test ("for-of loop");
         for E of V loop
            if Predicate (E) then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.End_Test;
         Assert (Co, Items_Count);

         Stdout.Start_Test ("count_if");
         Co := Count_If (V, Predicate'Access);
         Stdout.End_Test;
         Assert (Co, Items_Count);
      end Run;

   begin
      Stdout.Start_Container_Test ("Ada Array", "Integer Vector");
      for R in 1 .. Repeat_Count loop
         declare
            V     : Int_Array (1 .. Items_Count);
         begin
            Stdout.Save_Container_Size (V'Size / 8);  --  in bytes
            Run (V);
         end;
      end loop;
      Stdout.End_Container_Test;
   end Test_Arrays_Int;

end Perf_Support;
