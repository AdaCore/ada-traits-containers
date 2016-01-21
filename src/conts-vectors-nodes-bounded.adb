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
with System;       use System;

package body Conts.Vectors.Nodes.Bounded with SPARK_Mode is

   ----------
   -- Impl --
   ----------

   package body Impl is

      ---------------------
      -- Release_Element --
      ---------------------

      procedure Release_Element
        (Self : in out Container'Class; Index : Count_Type) is
      begin
         Elements.Release (Self.Nodes (Index));
      end Release_Element;

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element
        (Self    : in out Container'Class;
         Index   : Count_Type;
         Element : Elements.Stored_Type) is
      begin
         Self.Nodes (Index) := Element;
      end Set_Element;

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Self     : in out Container'Class;
         Source   : Container'Class;
         Last     : Count_Type)
      is
      begin
         if Elements.Copyable then
            --  We might have nothing to do
            if Self'Address /= Source'Address then
               Self.Nodes (Min_Index .. Last) :=
                 Source.Nodes (Min_Index .. Last);
            end if;
         else
            for J in Min_Index .. Last loop
               Self.Nodes (J) := Elements.Copy (Source.Nodes (J));
            end loop;
         end if;
      end Assign;
   end Impl;

end Conts.Vectors.Nodes.Bounded;
