--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body Memory is

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Current := (Total_Allocated => 0,
                  Allocs => 0,
                  Frees => 0,
                  Reallocs => 0);
   end Reset;

   ---------
   -- "-" --
   ---------

   function "-" (M1, M2 : Mem_Info) return Mem_Info is
   begin
      return (Total_Allocated => M1.Total_Allocated - M2.Total_Allocated,
              Allocs          => M1.Allocs - M2.Allocs,
              Frees           => M1.Frees - M2.Frees,
              Reallocs        => M1.Reallocs - M2.Reallocs);
   end "-";

   -----------
   -- Pause --
   -----------

   procedure Pause is
   begin
      Paused := True;
   end Pause;

   -------------
   -- Unpause --
   -------------

   procedure Unpause is
   begin
      Paused := False;
   end Unpause;
end Memory;
