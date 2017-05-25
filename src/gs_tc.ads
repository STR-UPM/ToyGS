------------------------------------------------------------------------------
-- $Id: gs_tm.ads 86 2017-05-04 14:09:17Z jpuente $
------------------------------------------------------------------------------
-- Project GS
-- GS_TC specification
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------

-- Telecommand subsystem
package GS_TC is

   -- Telecommands
   type TC_Type is (HK);

   -- Send Telecommand
   procedure Send (TC : TC_Type := HK);

   -- Initialize TC subsystem
   procedure Init;

end GS_TC;
