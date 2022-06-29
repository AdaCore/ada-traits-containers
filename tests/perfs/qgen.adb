--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body QGen is

   procedure Test_QGen is
      S : Sum_List;
   begin
      S.Append (Sum'(Id => 1, others => <>));
      S.Append (Sum'(Id => 2, others => <>));
      S.Append (Sum'(Id => 3, others => <>));

      --  Test requires that aspect Iterable supports unconstrained types
      --  for E of S loop
      --     Put_Line ("E=" & External_Tag (E'Tag));
      --  end loop;
   end Test_QGen;

end QGen;
