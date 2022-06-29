--
--  Copyright (C) 2016-2017, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
