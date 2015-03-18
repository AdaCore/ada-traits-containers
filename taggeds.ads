--  This package contains a simple list implemented via interfaces
--  and tagged types, only used for testing performance

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
