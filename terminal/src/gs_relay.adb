------------------------------------------------------------------------------
-- $Id: gs_relay.adb 99 2017-05-25 14:12:55Z jpuente $
------------------------------------------------------------------------------
-- Project GS - toy ground station
-- GS- relay main procedure body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with GS_Relay_TM;
with GS_Relay_TC;

with System;
with System.IO;
with Ada.Exceptions;

-- relay TCTM packets to another IP
procedure GS_Relay is
   pragma Priority (System.Priority'First);
begin
   loop
      null;
   end loop;
end GS_Relay;
