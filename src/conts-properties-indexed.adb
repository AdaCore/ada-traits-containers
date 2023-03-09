--
--  Copyright (C) 2016-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;

package body Conts.Properties.Indexed is

   use Value_Vectors;

   -----------
   -- Clear --
   -----------

   procedure Clear (M : in out Map) is
   begin
      M.Values.Clear;
   end Clear;

   ---------
   -- Get --
   ---------

   function Get (M : Map; K : Key_Type) return Element_Type is
   begin
      return M.Values.Element (Get_Index (K));
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (M : in out Map; K : Key_Type; Val : Element_Type) is
      Idx : constant Index_Type := Get_Index (K);
   begin
      --  ??? We should have such an operation in the vector directly
      if not (Value_Vectors.Vectors.To_Count (Idx) <= M.Values.Length) then
         M.Values.Resize
            (Length  => Value_Vectors.Vectors.To_Count (Idx),
             Element => Default_Value);
      end if;

      M.Values.Replace_Element (Idx, Val);
   end Set;

   ----------------
   -- Create_Map --
   ----------------

   function Create_Map (G : Container_Type) return Map is
   begin
      return M : Map do
         M.Values.Reserve_Capacity (Length (G));
      end return;
   end Create_Map;

end Conts.Properties.Indexed;
