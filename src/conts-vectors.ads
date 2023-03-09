--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;

package Conts.Vectors with SPARK_Mode => On is

   generic
      with function Grow
        (Current_Size, Min_Expected_Size : Count_Type) return Count_Type;
      with function Shrink
        (Current_Size, Min_Expected_Size : Count_Type) return Count_Type;
   package Resize_Strategy is
   end Resize_Strategy;
   --  This package is used whenever a vector needs to be resized, and
   --  must return the new size. There are two cases:
   --     Grow:
   --        Space for more elements must be added to the vector. A common
   --        strategy is to double the size, although it is also possible to
   --        chose to add a fixed number of elements.
   --     Shrink:
   --        The vector is too big for what it needs. In general, it should
   --        not immediately resize and free memory, in case elements are
   --        added just afterwards.
   --
   --  Current_Size might be 0, so simply multiplying is not enough.

   function Grow_1_5
     (Current_Size, Min_Expected : Count_Type) return Count_Type with Inline;
   function Shrink_1_5
     (Current_Size, Min_Expected : Count_Type) return Count_Type with Inline;
   package Resize_1_5 is new Resize_Strategy
     (Grow => Grow_1_5, Shrink => Shrink_1_5);
   --  A package that multiplies the size by 1.5 every time some more space
   --  is needed.

private

   function Grow_1_5
     (Current_Size, Min_Expected : Count_Type) return Count_Type
   is (if Current_Size < Min_Expected
       then  Count_Type'Max
         (Min_Expected, Count_Type'Max (4, Current_Size * 3 / 2))
       else Current_Size);

   function Shrink_1_5
     (Current_Size, Min_Expected : Count_Type) return Count_Type
   is (Min_Expected);

end Conts.Vectors;
