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

--  This package provides a specialization of the Element_Traits package for
--  use with Ada arrays.
--  It stores small arrays directly in the data, without requiring memory
--  allocations. On the other hand, for larger arrays it uses actual memory
--  allocations. This will in general result in much faster handling, since
--  calls to malloc are slow. On the other hand, this also uses more memory
--  since there is systematically space allocated for the small arrays, even
--  when we end up allocating memory.

pragma Ada_2012;

generic
   type Index_Type is new Integer;
   --  The index type for the array. For now, we need a type based on integer
   --  so that we can declare a specific subrange for small arrays

   type Element_Type is private;
   type Array_Type is array (Index_Type range <>) of Element_Type;

   with package Pool is new Conts.Pools (<>);
   --  Storage pool used to allocate memory

   Short_Size : Natural := Standard'Address_Size / Element_Type'Object_Size;
   --  Arrays below this size are stored inline (no memory allocation).
   --  Above this size, an actual allocation will be used.
   --  This size might have a big influence on performance, since it
   --  impact the processor cache. You should measure performance to
   --  find the optimal size in your case.
   --  The default value is chosen so that the Stored element has the
   --  same size whether we store the string inline or via a malloc.

package Conts.Elements.Arrays with SPARK_Mode => Off is

   type Constant_Ref_Type (Element : not null access constant Array_Type) is
      null record with Implicit_Dereference => Element;

   --  This package stores a string with explicit bounds, but is able to
   --  cast it as a standard string acces by simulating the fat pointers,
   --  while avoiding a copy of the string.
   package Fat_Pointers is
      type Fat_Pointer is private;
      procedure Set (FP : in out Fat_Pointer; A : Array_Type) with Inline;

      function Get
        (FP : not null access constant Fat_Pointer) return Constant_Ref_Type
         with Inline;
   private
      type Both_Bounds is record
         First, Last : Integer;
      end record;
      type Fat_Pointer is record
         Bounds : Both_Bounds;
         Data   : Array_Type (1 .. Index_Type (Short_Size));
      end record;
   end Fat_Pointers;

   package Impl is
      type Stored_Array is private;
      function To_Stored (A : Array_Type) return Stored_Array with Inline;
      function To_Ref (S : Stored_Array) return Constant_Ref_Type with Inline;
      function To_Element (R : Constant_Ref_Type) return Array_Type
         is (R.Element.all) with Inline;
      function Copy (S : Stored_Array) return Stored_Array with Inline;
      procedure Release (S : in out Stored_Array) with Inline;

   private
      type Storage_Kind is (Short_Array, Long_Array);

      type Array_Access is access all Array_Type;
      for Array_Access'Storage_Pool use Pool.Pool;

      type Stored_Array (Kind : Storage_Kind := Short_Array) is record
         case Kind is
            when Short_Array => Short : aliased Fat_Pointers.Fat_Pointer;
            when Long_Array  => Long  : Array_Access;
         end case;
      end record;
   end Impl;

   package Traits is new Conts.Elements.Traits
     (Element_Type           => Array_Type,
      Stored_Type            => Impl.Stored_Array,
      Returned_Type          => Constant_Ref_Type,
      Constant_Returned_Type => Constant_Ref_Type,
      To_Stored              => Impl.To_Stored,
      To_Returned            => Impl.To_Ref,
      To_Constant_Returned   => Impl.To_Ref,
      To_Element             => Impl.To_Element,
      Copy                   => Impl.Copy,
      Release                => Impl.Release,
      Copyable               => False,   --  would create aliases
      Movable                => True);

   function From_Ref_To_Element (R : Constant_Ref_Type) return Array_Type
      is (R.Element.all) with Inline;
   --  Convenience function for use in algorithms, to convert from a Ref_Type
   --  to an Element_Type. This is not needed in general, since the compiler
   --  will automatically (and efficiently) dereference the reference_type.
   --  But generic algorithm need an explicit conversion.

end Conts.Elements.Arrays;
