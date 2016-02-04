------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
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
with Ada.Containers; use Ada.Containers;

package body Conts.Maps is

   ------------------------
   -- Initialize_Probing --
   ------------------------

   overriding procedure Initialize_Probing
     (Self : in out Perturbation_Probing;
      Hash : Hash_Type;
      Size : Hash_Type)
   is
      pragma Unreferenced (Size);
   begin
      Self.Pertub := Hash;
   end Initialize_Probing;

   ------------------
   -- Next_Probing --
   ------------------

   overriding function Next_Probing
     (Self     : in out Perturbation_Probing;
      Previous : Hash_Type) return Hash_Type
   is
      Candidate : constant Hash_Type :=
        Previous * 4 + Previous + 1 + Self.Pertub;
   begin
      Self.Pertub := Self.Pertub / (2 ** 5);
      return Candidate;
   end Next_Probing;

end Conts.Maps;
