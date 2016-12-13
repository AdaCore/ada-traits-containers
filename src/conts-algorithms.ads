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
with Conts.Cursors;
with Conts.Properties;

package Conts.Algorithms is

   --------------
   -- Count_If --
   --------------

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Key_Type => Cursors.Cursor, others => <>);
   function Count_If_With_External_Get
     (Self      : Cursors.Container;
      Map       : Getters.Map;
      Predicate : not null access
        function (E : Getters.Element) return Boolean)
     return Natural
     with Global => null;
   --  Count the number of elements in the container that match the predicate

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Cursor,
         others   => <>);
   function Count_If
     (Self      : Cursors.Container;
      Predicate : not null access
        function (E : Getters.Element) return Boolean)
     return Natural
     with Global => null;
   --  Same as above, but the container itself is the property map to
   --  retrieve the element

   -------------
   -- Shuffle --
   -------------

   generic
      with package Cursors is new Conts.Cursors.Random_Access_Cursors (<>);
      with package Random is new Conts.Uniform_Random_Traits
        (Discrete_Type => Cursors.Index, others => <>);
      with procedure Swap
        (Self        : in out Cursors.Container;
         Left, Right : Cursors.Index) is <>;
   procedure Shuffle
     (Self : in out Cursors.Container;
      Gen  : in out Random.Generator)
     with Global => null;
   --  Generates a random permutation of Self.
   --  If you 'use' the package for your container (vector for instance), then
   --  Swap will generally be visible by default.
   --  Complexity: O(n)

   -------------
   -- Subsets --
   -------------
   --  This package can be used to manipulate subsets of containers in the
   --  algorithms.
   --  It has the same formal parameters as a sort algorithm, so that you can
   --  more easily sort only a range.

   generic
      with package Base_Cursors is
        new Conts.Cursors.Random_Access_Cursors (<>);
      with package Base_Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Base_Cursors.Container,
         Key_Type => Base_Cursors.Index,
         others   => <>);
      with procedure Base_Swap
        (Self        : in out Base_Cursors.Container;
         Left, Right : Base_Cursors.Index) is <>;
   package Ranged_Random_Access_Cursors is
      type Rg is record
         Base     : not null access Base_Cursors.Container;
         From, To : Base_Cursors.Index;
      end record;

      function Subset
        (Self     : not null access Base_Cursors.Container;
         From, To : Base_Cursors.Index)
         return Rg
      is (Base => Self, From => From, To => To);
      --  Maps a subset of a container, [From..To].
      --  The result is only valid while Self itself is valid, since no copy
      --  is made. The subset works on Self's data directly.

      function First (Self : Rg) return Base_Cursors.Index is (Self.From);
      function Last (Self : Rg) return Base_Cursors.Index is (Self.To);

      package Cursors is new Conts.Cursors.Random_Access_Cursors
        (Container_Type     => Rg,
         Index_Type         => Base_Cursors.Index,
         No_Element         => Base_Cursors.No_Element,
         First              => First,
         Last               => Last,
         Distance           => Base_Cursors.Distance,
         "+"                => Base_Cursors."+");

      function Get (M : Rg; K : Cursors.Index) return Base_Getters.Element
      is (Base_Getters.Get (M.Base.all, K));

      package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type           => Rg,
         Key_Type           => Cursors.Index,
         Element_Type       => Base_Getters.Element,
         Get                => Get);

      procedure Swap (Self : in out Rg; Left, Right :  Cursors.Index)
        with Inline, Global => null;
   end Ranged_Random_Access_Cursors;

   ----------
   -- Sort --
   ----------

   generic
      with package Cursors is new Conts.Cursors.Random_Access_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Index,
         others   => <>);
      with function "<" (Left, Right : Getters.Element) return Boolean is <>;
      with procedure Swap
        (Self        : in out Cursors.Container;
         Left, Right : Cursors.Index) is <>;
   procedure Insertion_Sort (Self : in out Cursors.Container)
     with Global => null;
   --  Sort the container.
   --  This is an algorithm only suitable for small containers (up to say 100
   --  elements for instance), for which it is fast since it has low overhead.
   --
   --  Stable: when two elements compare equal, their initial order is
   --     preserved. You can thus do a first sort for one criteria, then
   --     another sort with another criteria, and elements will be sorted based
   --     on the two criterias.
   --  In-Place: no additional storage required.
   --  Adaptive: it executes faster when Self is already partially sorted.
   --
   --  Complexity:
   --     - if Self is already sorted, this is O(n)
   --     - worst-case execution is O(n^2)

   type Shell_Sort_Gaps is array (Natural range <>) of Integer;
   Ciura_Gaps : constant Shell_Sort_Gaps :=
     (1, 4, 10, 23, 57, 132, 301, 701);
   Sedgewick_Gaps : constant Shell_Sort_Gaps :=
     (1, 8, 23, 77, 281, 1073, 4193, 16577, 65921, 262913);

   generic
      with package Cursors is new Conts.Cursors.Random_Access_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Index,
         others   => <>);
      with function "<" (Left, Right : Getters.Element) return Boolean is <>;
      with procedure Swap
        (Self        : in out Cursors.Container;
         Left, Right : Cursors.Index) is <>;
      Gaps : Shell_Sort_Gaps := Ciura_Gaps;
   procedure Shell_Sort
     (Self : in out Cursors.Container)
     with Global => null;
   --  Sort the container.
   --  This is an improvement over Insertion_Sort. It does several iterations
   --  of Insertion_Sort, each looking lookip at elements apart from each other
   --  based on the Gaps settings. The Wiki page for this algorithm suggests
   --  a number of possible gaps, the default is usually a good choice.
   --
   --  Unstable: equal elements might change order.
   --  Adaptive: it executes faster if Self is already partially sorted.
   --  In-place: no additional storage required.
   --
   --  This algorithm needs to move a cursor by a large number places, so is
   --  not suitable for lists (although it will work).
   --
   --  Complexity:
   --     - if Self is already sorted, this is O(n)
   --     - worst case execution is roughly O(n^4/3), though it depends on the
   --       gap sequence chosen in the instantiation.

   generic
      with package Cursors is new Conts.Cursors.Random_Access_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Index,
         others   => <>);
      with function "<" (Left, Right : Getters.Element) return Boolean is <>;
      with procedure Swap
        (Self        : in out Cursors.Container;
         Left, Right : Cursors.Index) is <>;
      Threshold  : Integer := 20;
   procedure Quicksort (Self : in out Cursors.Container)
     with Global => null;
   --  Sort the container.
   --  When there are fewer than Threshold elements in a sequence, a simpler
   --  sort is used instead to avoid a lot of recursions.
   --
   --  Unstable: equal elements might change order.
   --  In-place: no additional storage requirement
   --
   --  Complexity:
   --      - O(n*log(n)) on average
   --      - O(n^2) worst case

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Cursor,
         others   => <>);
      with function "<" (Left, Right : Getters.Element) return Boolean is <>;
   function Is_Sorted (Self : Cursors.Container) return Boolean
     with Global => null;
   --  Whether Self is sorted for the given criteria.
   --  On exit, an element is always "<" or equal to the following element.

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
   function Find
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Cursors.Cursor
     with Global => null;
   --  Return the location of E within Self, or No_Element if it could not be
   --  found.

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
   function Contains
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Boolean
     with Global => null;
   --  True if E is found in Self

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
   function Equals (Left, Right  : Cursors.Container) return Boolean
     with Global => null;
   --  True if Left and Right contain the same elements, in the same order.

end Conts.Algorithms;
