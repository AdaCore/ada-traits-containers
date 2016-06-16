------------------------------------------------------------------------------
--                     Copyright (C) 2016-2016, AdaCore                     --
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
