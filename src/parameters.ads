--------------------------------------------------------------------------------
-- $Id: parameters.ads 86 2017-05-04 14:09:17Z jpuente $
-- Project GS
-- GS parameters specification
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
-------------------------------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;
with GNAT.Sockets;  use GNAT.Sockets;
package Parameters is

   -- Communication ports for TC/TM.
   -- The values below are defaults that can be changed at start time
   -- by command line parameters (see gs.ads).

   -- Local port for receiving TM
   TM_Port   : Port_Type := 8485;

   -- Remote port to which TC messages are sent
   OBSW_IP   : Inet_Addr_Type := Inet_Addr("127.0.0.1");
   OBSW_Port : Port_Type      := 8484;        -- remote port for sending TC

   -- Start time for all tasks. Allow for some delay in order to
   -- make sure that all the components are properly initialized
   Start_Time : Time := Clock + Milliseconds(100);

end Parameters;
