--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Ada.Containers; use Ada.Containers;

package body Conts.Maps is

   ------------------------
   -- Initialize_Probing --
   ------------------------

   overriding procedure Initialize_Probing
     (Self : in out Perturbation_Probing;
      Hash : Hash_Type;
      Size : Hash_Type)
   is
      pragma Unreferenced (Size);
   begin
      Self.Pertub := Hash;
   end Initialize_Probing;

   ------------------
   -- Next_Probing --
   ------------------

   overriding function Next_Probing
     (Self     : in out Perturbation_Probing;
      Previous : Hash_Type) return Hash_Type
   is
      Candidate : constant Hash_Type :=
        Previous * 4 + Previous + 1 + Self.Pertub;
   begin
      Self.Pertub := Self.Pertub / (2 ** 5);
      return Candidate;
   end Next_Probing;

end Conts.Maps;
