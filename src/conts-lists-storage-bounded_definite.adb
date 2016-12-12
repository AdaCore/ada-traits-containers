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

package body Conts.Lists.Storage.Bounded_Definite with SPARK_Mode => Off is

   package body Impl is
      --------------
      -- Allocate --
      --------------

      procedure Allocate
         (Self    : in out Container'Class;
          Element : Stored_Type;
          N       : out Node_Access)
      is
      begin
         if Self.Free > 0 then
            N := Node_Access (Self.Free);
            Self.Free := Integer (Self.Nodes (Count_Type (N)).Next);
         else
            N := Node_Access (abs Self.Free + 1);
            Self.Free := Self.Free - 1;
         end if;

         if Count_Type (N) <= Self.Nodes'Last then
            Self.Nodes (Count_Type (N)) :=
               (Element  => Element,
                Previous => Null_Node_Access,
                Next     => Null_Node_Access);
         else
            N := Null_Node_Access;
         end if;
      end Allocate;

      -----------------
      -- Get_Element --
      -----------------

      function Get_Element
         (Self : Container'Class; N : Node_Access) return Stored_Type is
      begin
         return Self.Nodes (Count_Type (N)).Element;
      end Get_Element;

      --------------
      -- Get_Next --
      --------------

      function Get_Next
         (Self : Container'Class; N : Node_Access) return Node_Access is
      begin
         return Self.Nodes (Count_Type (N)).Next;
      end Get_Next;

      ------------------
      -- Get_Previous --
      ------------------

      function Get_Previous
         (Self : Container'Class; N : Node_Access) return Node_Access is
      begin
         return Self.Nodes (Count_Type (N)).Previous;
      end Get_Previous;

      ------------------
      -- Set_Previous --
      ------------------

      procedure Set_Previous
         (Self : in out Container'Class; N, Prev : Node_Access) is
      begin
         Self.Nodes (Count_Type (N)).Previous := Prev;
      end Set_Previous;

      --------------
      -- Set_Next --
      --------------

      procedure Set_Next
         (Self : in out Container'Class; N, Next : Node_Access) is
      begin
         Self.Nodes (Count_Type (N)).Next := Next;
      end Set_Next;

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element
        (Self : in out Impl.Container'Class;
         N    : Node_Access;
         E    : Stored_Type) is
      begin
         Self.Nodes (Count_Type (N)).Element := E;
      end Set_Element;

      ------------
      -- Assign --
      ------------

      procedure Assign
         (Nodes    : in out Container'Class;
          Source   : Container'Class;
          New_Head : out Node_Access;
          Old_Head : Node_Access;
          New_Tail : out Node_Access;
          Old_Tail : Node_Access) is
      begin
         --  Indices will remain the same
         New_Head := Old_Head;
         New_Tail := Old_Tail;
         Nodes.Free  := Source.Free;
         Nodes.Nodes := Source.Nodes;
      end Assign;

   end Impl;

end Conts.Lists.Storage.Bounded_Definite;
