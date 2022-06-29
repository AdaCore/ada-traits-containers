--
--  Copyright (C) 2016-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides wrappers around SPARK compatible algorithms of
--  Conts.Algorithms providing postconditions.
--  They should be instanciated with appropriate models
--  of the container. More precisely, for every container Self, the
--  result of Content.Model (Self) must be such that Content.Get (Content.Model
--  (Self), Content.First + I) is always the element returned by
--  Getters.Get (Self, C) on the cursor C obtained by applying Cursors.Next
--  I times on Cursors.First (Self).
--  These algorithms have a body with SPARK_Mode => Off so they must be
--  instantiated at library level inside SPARK code.

pragma Ada_2012;
with Conts.Cursors;
with Conts.Properties;
with Conts.Properties.SPARK;

package Conts.Algorithms.SPARK is

   ----------
   -- Find --
   ----------

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Cursor,
         others   => <>);
      with function "=" (K1, K2 : Getters.Element) return Boolean is <>;
      with package Content is new Conts.Properties.SPARK.Content_Models
        (Map_Type     => Getters.Map,
         Element_Type => Getters.Element_Type,
         others       => <>);
   function Find
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Cursors.Cursor
     with SPARK_Mode,
     Global         => null,
     Contract_Cases =>
       ((for all I in Content.First .. Content.Last (Content.Model (Self)) =>
            Content.Get (Content.Model (Self), I) /= E)  =>
          Cursors."=" (Find'Result, Cursors.No_Element),
        others => Cursors.Has_Element (Self, Find'Result)
        and then Getters.Get (Self, Find'Result) = E);

   --------------
   -- Contains --
   --------------

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Cursor,
         others   => <>);
      with function "=" (K1, K2 : Getters.Element) return Boolean is <>;
      with package Content is new Conts.Properties.SPARK.Content_Models
        (Map_Type     => Getters.Map,
         Element_Type => Getters.Element_Type,
         others       => <>);
   function Contains
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Boolean
     with SPARK_Mode,
     Global => null,
     Post   => Contains'Result =
       (for some I in Content.First .. Content.Last (Content.Model (Self)) =>
          Content.Get (Content.Model (Self), I) = E);

   ------------
   -- Equals --
   ------------

   generic
      with package Cursors is new Conts.Cursors.Random_Access_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Index_Type,
         others   => <>);
      with function "=" (K1, K2 : Getters.Element) return Boolean is <>;
      with package Content is new Conts.Properties.SPARK.Content_Models
        (Map_Type     => Getters.Map,
         Element_Type => Getters.Element_Type,
         others       => <>);
   function Equals (Left, Right  : Cursors.Container) return Boolean
     with SPARK_Mode,
     Global => null,
     Post   => Equals'Result =
       (Content."=" (Content.Last (Content.Model (Left)),
        Content.Last (Content.Model (Right)))
        and then
          (for all I in Content.First .. Content.Last (Content.Model (Left)) =>
             Content.Get (Content.Model (Left), I) =
               Content.Get (Content.Model (Right), I)));

end Conts.Algorithms.SPARK;
