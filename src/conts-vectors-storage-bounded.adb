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

package body Conts.Vectors.Storage.Bounded with SPARK_Mode => Off is

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
        (Self                : in out Container'Class;
         Source              : Container'Class;
         Last                : Count_Type) is
      begin
         Copy (Self, Source, Min_Index, Last, Min_Index);
      end Assign;

      ----------
      -- Copy --
      ----------

      procedure Copy
        (Self                   : in out Container'Class;
         Source                 : Container'Class;
         Source_From, Source_To : Count_Type;
         Self_From              : Count_Type) is
      begin
         if Elements.Copyable then
            Self.Nodes (Self_From .. Self_From + Source_To - Source_From) :=
              Source.Nodes (Source_From .. Source_To);
         else
            for J in Source_From .. Source_To loop
               Self.Nodes (Self_From + J - Source_From) :=
                 Elements.Copy (Source.Nodes (J));
            end loop;
         end if;
      end Copy;
   end Impl;

end Conts.Vectors.Storage.Bounded;
