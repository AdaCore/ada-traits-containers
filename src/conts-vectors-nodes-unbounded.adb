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
with Ada.Unchecked_Conversion;
with System;                   use System;
with System.Memory;            use System.Memory;

package body Conts.Vectors.Nodes.Unbounded is

   package body Impl is
      pragma Warnings (Off);  --  no aliasing issue
      function Convert is new Ada.Unchecked_Conversion
        (Nodes_Array_Access, System.Address);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Nodes_Array_Access);
      pragma Warnings (On);

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
         S : constant size_t := size_t
           (Source.Capacity * Source.Nodes'Component_Size
            / System.Storage_Unit);

         --  Use a temporary vector in case Self is the same as Source
         Tmp : Nodes_Array_Access;
      begin
         Tmp := Convert (System.Memory.Alloc (S));

         if Elements.Copyable then
            Tmp (Min_Index .. Last) := Source.Nodes (Min_Index .. Last);
         else
            for J in Min_Index .. Last loop
               Tmp (J) := Elements.Copy (Source.Nodes (J));
            end loop;
         end if;

         Self.Nodes := Tmp;
         Self.Capacity := Source.Capacity;
      end Assign;

      ------------
      -- Resize --
      ------------

      procedure Resize
        (Self     : in out Container'Class;
         New_Size : Count_Type;
         Last     : Count_Type;
         Force    : Boolean)
      is
         Size : Count_Type;
         S   : size_t;
         Tmp : Nodes_Array_Access;
      begin
         if Force then
            Size := New_Size;
         elsif New_Size < Self.Capacity then
            Size := Resize_Policy.Shrink
              (Current_Size => Self.Capacity, Min_Expected_Size => New_Size);
         else
            Size := Resize_Policy.Grow
              (Current_Size => Self.Capacity, Min_Expected_Size => New_Size);
         end if;

         if Size /= Self.Capacity then
            if Size = 0 then
               System.Memory.Free (Convert (Self.Nodes));
               Self.Nodes := null;
            else
               S := size_t
                 (Size * Self.Nodes'Component_Size / System.Storage_Unit);

               if Self.Nodes = null then
                  Self.Nodes := Convert (System.Memory.Alloc (S));

               elsif Elements.Movable then
                  Self.Nodes := Convert (Realloc (Convert (Self.Nodes), S));

               else
                  Tmp := Convert (System.Memory.Alloc (S));

                  for J in Min_Index .. Count_Type'Min (Last, New_Size) loop
                     Tmp (J) := Elements.Copy (Self.Nodes (J));
                     Elements.Release (Self.Nodes (J));
                  end loop;

                  System.Memory.Free (Convert (Self.Nodes));
                  Self.Nodes := Tmp;
               end if;
            end if;

            Self.Capacity := Size;
         end if;
      end Resize;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Container'Class) is
      begin
         System.Memory.Free (Convert (Self.Nodes));
         Self.Nodes := null;
         Self.Capacity := 0;
      end Release;

   end Impl;

end Conts.Vectors.Nodes.Unbounded;
