--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Ada.Containers;   use Ada.Containers;

package Conts.Maps is

   -------------
   -- Probing --
   -------------
   --  The implementation of the hashed map stores all elements in a single
   --  array. Preferably, the bucket used is the one corresponding to the hash
   --  computed from the key. But in case there is already another element at
   --  that position, other positions have to be tried. There are multiple
   --  strategies for this.
   --  We use a tagged record for this, since some strategies need to keep
   --  data. A new instance of the object is created and initialized every time
   --  we search for a new key.

   type Probing_Strategy is interface;
   --  An object whose goal is to compute the next candidate

   procedure Initialize_Probing
     (Self : in out Probing_Strategy;
      Hash : Hash_Type;
      Size : Hash_Type) is null;
   --  Called once when a lookup starts

   function Next_Probing
     (Self     : in out Probing_Strategy;
      Previous : Hash_Type) return Hash_Type is abstract;
   --  Compute the next position to check, given we checked Previous and found
   --  this position already in use.

   --------------------
   -- Linear probing --
   --------------------
   --  Simple probing: check the next place in the array. This is simple,
   --  but not optimal in general when the keys are sequential integers for
   --  instance, since we end up with blocks of filled slots, which slows the
   --  lookup.

   type Linear_Probing is new Probing_Strategy with null record;
   overriding function Next_Probing
     (Self : in out Linear_Probing; Previous : Hash_Type) return Hash_Type
     is (Previous + 1) with Inline;

   -------------------------
   -- Pertubation_Probing --
   -------------------------
   --  Similar to linear probing, but more efficient since it will try various
   --  places in the array.

   type Perturbation_Probing is new Probing_Strategy with private;
   overriding procedure Initialize_Probing
     (Self : in out Perturbation_Probing;
      Hash : Hash_Type;
      Size : Hash_Type) with Inline;
   overriding function Next_Probing
     (Self     : in out Perturbation_Probing;
      Previous : Hash_Type) return Hash_Type
     with Inline;

   ---------------------
   -- Resize strategy --
   ---------------------

   function Resize_2_3
     (Used     : Count_Type;
      Fill     : Count_Type;
      Capacity : Count_Type) return Count_Type
     is (Count_Type
           (Hash_Type'Min
              ((if Hash_Type (Fill) > (Hash_Type (Capacity) * 2) / 3
                then (if Used > 100_000
                      then Hash_Type (Used) * 2
                      else Hash_Type (Used) * 4)
                else 0),   --  no resizing in this case
                Hash_Type (Count_Type'Last))))
     with Inline;
   --  This strategy attempts to keep the table at most 2/3. If this isn't the
   --  case, the size of the table is multiplied by 4 (which trades memory for
   --  efficiency by limiting the number of mallocs). However, when the table
   --  is already large, we only double the size.
   --
   --  If memory is more important than pure speed for you, you could modify
   --  this strategy.
   --
   --  The actual size allocated for the table will be the nearest power of 2
   --  greater than the returned value.
   --
   --  See Conts.Maps.Generics.Resize_Strategy for more information on the
   --  parameters.

private

   type Perturbation_Probing is new Probing_Strategy with record
      Pertub : Hash_Type;
   end record;

end Conts.Maps;
