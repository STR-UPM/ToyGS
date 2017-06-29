------------------------------------------------------------------------------
-- $Id: gs_relay_tc.ads 99 2017-05-25 14:12:55Z jpuente $
------------------------------------------------------------------------------
-- Project GS - toy ground station
-- GS- relay TC spec
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with GNAT.Sockets;     use GNAT.Sockets;
with Ada.Streams;

with System.IO;
with Ada.Exceptions;

-- Relay TC packets
--
package GS_Relay_TC is

  task TC_Relay;

end GS_Relay_TC;
