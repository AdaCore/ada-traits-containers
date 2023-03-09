--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  A special kind of elements that store nothing.
--  This is only useful to instantiate some containers, for instance a graph,
--  when no extra information needs to be added to the vertices.

pragma Ada_2012;

package Conts.Elements.Null_Elements is

   type Null_Element is null record;

   No_Element : constant Null_Element := (others => <>);

   function Identity (E : Null_Element) return Null_Element is (E) with Inline;
   package Traits is new Conts.Elements.Traits
     (Element_Type            => Null_Element,
      Stored_Type             => Null_Element,
      Returned_Type           => Null_Element,
      Constant_Returned_Type  => Null_Element,
      Copyable                => True,
      Movable                 => True,
      To_Stored               => Identity,
      To_Returned             => Identity,
      To_Constant_Returned    => Identity,
      To_Element              => Identity,
      Copy                    => Identity);

end Conts.Elements.Null_Elements;
