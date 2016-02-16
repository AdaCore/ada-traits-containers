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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

package body Conts.Elements.Arrays is

   package body Fat_Pointers is
      type C_Fat_Pointer is record
         Data, Bounds : System.Address;
      end record;

      type Array_Access is access all Array_Type;
      pragma No_Strict_Aliasing (Array_Access);

      pragma Warnings (Off);   --  strict aliasing
      function To_FP is new Ada.Unchecked_Conversion
         (C_Fat_Pointer, Array_Access);
      pragma Warnings (On);   --  strict aliasing

      procedure Set (FP : in out Fat_Pointer; A : Array_Type) is
      begin
         FP.Data (1 .. A'Length) := A;
         FP.Bounds := (1, A'Length);
      end Set;

      function Get
         (FP : not null access constant Fat_Pointer) return Constant_Ref_Type
      is
         F  : constant C_Fat_Pointer := (FP.Data'Address, FP.Bounds'Address);
         AC : constant Array_Access := To_FP (F);
      begin
         return Constant_Ref_Type'(Element => AC);
      end Get;
   end Fat_Pointers;

   package body Impl is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
          (Array_Type, Array_Access);

      ---------------
      -- To_Stored --
      ---------------

      function To_Stored (A : Array_Type) return Stored_Array is
      begin
         if A'Length <= Short_Size then
            return S : Stored_Array (Short_Array) do
               Fat_Pointers.Set (S.Short, A);
            end return;
         else
            return S : Stored_Array (Long_Array) do
               S.Long := new Array_Type'(A);
            end return;
         end if;
      end To_Stored;

      ------------
      -- To_Ref --
      ------------

      function To_Ref (S : Stored_Array) return Constant_Ref_Type is
      begin
         if S.Kind = Short_Array then
            return Fat_Pointers.Get (S.Short'Access);
         else
            return Constant_Ref_Type'(Element => S.Long);
         end if;
      end To_Ref;

      ----------
      -- Copy --
      ----------

      function Copy (S : Stored_Array) return Stored_Array is
      begin
         case S.Kind is
            when Short_Array =>
               return S;
            when Long_Array =>
               return R : Stored_Array (Long_Array) do
                  R.Long := new Array_Type'(S.Long.all);
               end return;
         end case;
      end Copy;

      -------------
      -- Release --
      -------------

      procedure Release (S : in out Stored_Array) is
      begin
         if S.Kind = Long_Array then
            Unchecked_Free (S.Long);
         end if;
      end Release;
   end Impl;

end Conts.Elements.Arrays;
