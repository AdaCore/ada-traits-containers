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

--  This package contains a simple list implemented via interfaces
--  and tagged types, only used for testing performance of having a lot
--  of dynamic dispatching.
--  Such containers are slower, and the algorithms cannot so easily be
--  adapted to other user-defined containers.
--
--  The primitive operations on cursor do not receive the container as
--  a parameter, which means this cannot be used for formal containers.

with Ada.Containers.Doubly_Linked_Lists;

generic
   type T is private;
package Taggeds is
   type Forward_Cursor is interface;
   function Element (C : Forward_Cursor) return T is abstract;
   procedure Next (C : in out Forward_Cursor) is abstract;
   function Has_Element (C : Forward_Cursor) return Boolean is abstract;

   type Container is interface;
   function First (C : Container) return Forward_Cursor'Class is abstract;
   procedure Append (C : in out Container; P : T) is abstract;

   package Internal_Lists is new Ada.Containers.Doubly_Linked_Lists (T);

   type List is new Container with record
      L : Internal_Lists.List;
   end record;
   overriding function First (C : List) return Forward_Cursor'Class;
   overriding procedure Append (C : in out List; P : T);

   type List_Cursor is new Forward_Cursor with record
      C : Internal_Lists.Cursor;
   end record;
   overriding function Element (C : List_Cursor) return T;
   overriding procedure Next (C : in out List_Cursor);
   overriding function Has_Element (C : List_Cursor) return Boolean;
end Taggeds;
