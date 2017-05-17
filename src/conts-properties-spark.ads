------------------------------------------------------------------------------
--                     Copyright (C) 2016-2017, AdaCore                     --
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

--  This package describes the concept of property models. They are used to
--  annotate containers. Models of a map are sequences of elements indexed
--  by a discrete type. For ease of use, the content models property is
--  instantiated in the spark version of containers.

pragma Ada_2012;

package Conts.Properties.SPARK is

   -----------------------------
   -- Property content models --
   -----------------------------

   generic
      type Map_Type (<>) is limited private;
      type Element_Type (<>) is private;
      type Model_Type is private;
      type Index_Type is (<>);
      with function Model (M : Map_Type) return Model_Type;
      with function Get (M : Model_Type; I : Index_Type) return Element_Type;
      with function First return Index_Type;
      with function Last (M : Model_Type) return Index_Type;
   package Content_Models with Ghost is
      subtype Map is Map_Type;
      subtype Element is Element_Type;
   end Content_Models;

end Conts.Properties.SPARK;
