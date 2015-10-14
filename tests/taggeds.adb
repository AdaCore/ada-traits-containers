------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
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

package body Taggeds is
   overriding function First (C : List) return Forward_Cursor'Class is
   begin
      return List_Cursor'(C => C.L.First);
   end First;

   overriding procedure Append (C : in out List; P : T) is
   begin
      Internal_Lists.Append (C.L, P);
   end Append;

   overriding function Element (C : List_Cursor) return T is
   begin
      return Internal_Lists.Element (C.C);
   end Element;

   overriding procedure Next (C : in out List_Cursor) is
   begin
      Internal_Lists.Next (C.C);
   end Next;

   overriding function Has_Element (C : List_Cursor) return Boolean is
   begin
      return Internal_Lists.Has_Element (C.C);
   end Has_Element;
end Taggeds;
