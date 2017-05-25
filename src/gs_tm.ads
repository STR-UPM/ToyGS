------------------------------------------------------------------------------
-- $Id: gs_tm.ads 86 2017-05-04 14:09:17Z jpuente $
------------------------------------------------------------------------------
-- Project GS
-- GS_TM specification
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------

-- Telemetry subsystem
package GS_TM is

   -- Telemetry message type
   type TM_Type is (Basic, HK);

   -- Initialize TM sybsystem
   procedure Init;

end GS_TM;
