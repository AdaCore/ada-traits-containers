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
with System;                    use System;
with System.Memory;             use System.Memory;
with System.Unsigned_Types;     use System.Unsigned_Types;

package body Conts.Lists.Storage.Unbounded_SPARK with SPARK_Mode is

   package body Private_Nodes_List with SPARK_Mode => Off is
      pragma Warnings (Off);  --  no aliasing issue
      function Convert is new Ada.Unchecked_Conversion
        (Nodes_Array_Access, System.Address);
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Nodes_Array_Access);
      pragma Warnings (On);

      --------------
      -- Allocate --
      --------------

      procedure Allocate
        (Self    : in out Nodes_List'Class;
         Element : Elements.Stored_Type;
         N       : out Node_Access)
      is
         New_Size : System.Unsigned_Types.Unsigned := 4;
         S : size_t;
      begin
         --  Reuse empty slots if possible
         if Self.Free > 0 then
            N := Node_Access (Self.Free);
            Self.Free := Integer (Self.Nodes (Count_Type (N)).Next);
         else
            N := Node_Access (abs Self.Free + 1);
            Self.Free := Self.Free - 1;
         end if;

         --  Grow the table of nodes if needed

         if Count_Type (N) > Self.Last then
            --  Use the same allocation scheme as in python, in an effort to
            --  find a good tradeoff to allocate a lot of memory and be
            --  efficient. Growth pattern is 0, 4, 8, 16, 25, 35, 46, 58,
            --  72, 88, 106, 126, 148, 173, 201, 233, 269, 309, 354, 405,...
            --  The over-allocation is mild, but is enough to give
            --  linear-time amortized behavior of a long sequence of
            --  appends.
            --
            --  Performance: adding 300_000 items with this scheme:
            --       265% of the time needed for C++ STL
            --  With the scheme were the size is multiplied by 1.5:
            --       137% of the time needed for C++ STL
            --  When multiplying by 2: similar to multiplying by 1.5

            --  New_Size := Unsigned (N);   --  minimal needed size
            --  New_Size := New_Size + Shift_Right (New_Size, 3) +
            --     (if New_Size < 9 then 3 else 6);

            New_Size := Unsigned'Max
              (Unsigned (Count_Type'Max (Self.Last, 1) * 3 / 2),
               Unsigned (N));
            Self.Last := Count_Type (New_Size);

            S := size_t (Self.Last * Node'Size / System.Storage_Unit);

            if Self.Nodes = null then
               Self.Nodes := Convert (Alloc (S));
            else
               Self.Nodes := Convert (Realloc (Convert (Self.Nodes), S));
               --   ??? if not movable, need to do something
            end if;
         end if;

         Self.Nodes (Count_Type (N)) :=
           (Element  => Element,
            Previous => Null_Node_Access,
            Next     => Null_Node_Access);
      end Allocate;

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Nodes    : in out Nodes_List'Class;
         Source   : Nodes_List'Class;
         New_Head : out Node_Access;
         Old_Head : Node_Access;
         New_Tail : out Node_Access;
         Old_Tail : Node_Access)
      is
         S : size_t;
         N : Node_Access;
      begin
         --  We *must* preserve the indices
         New_Head := Old_Head;
         New_Tail := Old_Tail;

         S := size_t
           (Source.Last * Source.Nodes'Component_Size
            / System.Storage_Unit);
         Nodes.Nodes := Convert (Alloc (S));

         Nodes.Last := Source.Last;
         Nodes.Free := Source.Free;

         if Elements.Copyable then
            Nodes.Nodes (1 .. Nodes.Last) := Source.Nodes (1 .. Source.Last);
         else
            N := Old_Head;
            while N /= Null_Node_Access loop
               declare
                  Value : Node renames Source.Nodes (Count_Type (N));
               begin
                  Nodes.Nodes (Count_Type (N)) :=
                    (Element  => Elements.Copy (Value.Element),
                     Next     => Value.Next,
                     Previous => Value.Previous);
                  N := Value.Next;
               end;
            end loop;
         end if;
      end Assign;

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Nodes_List'Class) is
      begin
         System.Memory.Free (Convert (Self.Nodes));
         Self.Nodes := null;
         Self.Last := 0;
         Self.Free := 0;
      end Release;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next
        (Self : in out Nodes_List'Class; N, Next : Node_Access) is
      begin
         Self.Nodes (Count_Type (N)).Next := Next;
      end Set_Next;

      ------------------
      -- Set_Previous --
      ------------------

      procedure Set_Previous
        (Self : in out Nodes_List'Class; N, Previous : Node_Access) is
      begin
         Self.Nodes (Count_Type (N)).Previous := Previous;
      end Set_Previous;

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element
        (Self : in out Nodes_List'Class;
         N    : Node_Access;
         E    : Elements.Stored_Type) is
      begin
         Self.Nodes (Count_Type (N)).Element := E;
      end Set_Element;

   end Private_Nodes_List;

end Conts.Lists.Storage.Unbounded_SPARK;
