--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;

package body Conts.Vectors.Storage.Bounded with SPARK_Mode => Off is

   ----------
   -- Impl --
   ----------

   package body Impl is

      ---------------------
      -- Release_Element --
      ---------------------

      procedure Release_Element
        (Self : in out Container'Class; Index : Count_Type) is
      begin
         Elements.Release (Self.Nodes (Index));
      end Release_Element;

      -----------------
      -- Set_Element --
      -----------------

      procedure Set_Element
        (Self    : in out Container'Class;
         Index   : Count_Type;
         Element : Elements.Stored_Type) is
      begin
         Self.Nodes (Index) := Element;
      end Set_Element;

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Self                : in out Container'Class;
         Source              : Container'Class;
         Last                : Count_Type) is
      begin
         Copy (Self, Source, Min_Index, Last, Min_Index);
      end Assign;

      ----------
      -- Copy --
      ----------

      procedure Copy
        (Self                   : in out Container'Class;
         Source                 : Container'Class;
         Source_From, Source_To : Count_Type;
         Self_From              : Count_Type) is
      begin
         if Elements.Copyable then
            Self.Nodes (Self_From .. Self_From + Source_To - Source_From) :=
              Source.Nodes (Source_From .. Source_To);
         else
            for J in Source_From .. Source_To loop
               Self.Nodes (Self_From + J - Source_From) :=
                 Elements.Copy (Source.Nodes (J));
            end loop;
         end if;
      end Copy;
   end Impl;

end Conts.Vectors.Storage.Bounded;
