--
--  Copyright (C) 2016-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Report;             use Report;

package Custom_Graph is
   procedure Test_Custom (Stdout : not null access Output'Class);
   procedure Test_Adjacency_List (Stdout : not null access Output'Class);
end Custom_Graph;
