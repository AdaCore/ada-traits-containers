--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Vectors;
with Lists;
with Maps;

--  This test should be run with assertions enables (in Debug mode). It should
--  exercise the postconditions defined on container subprograms.

procedure Main is
begin
   Vectors;
   Lists;
   Maps;
end Main;
