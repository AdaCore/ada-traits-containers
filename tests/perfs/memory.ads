--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package Memory is

   type Mem_Info is record
      Total_Allocated : Long_Long_Integer := 0;
      Allocs          : Integer := 0;
      Frees           : Integer := 0;
      Reallocs        : Integer := 0;
   end record;

   Current : Mem_Info;

   function "-" (M1, M2 : Mem_Info) return Mem_Info;
   --  Compute the delta between two memory usage

   Paused : Boolean := False;

   procedure Reset;

   procedure Pause;
   --  Stop counting allocs and frees

   procedure Unpause;
   --  Resume counting
end Memory;
