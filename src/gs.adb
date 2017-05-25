------------------------------------------------------------------------------
-- $Id: gs.adb 86 2017-05-04 14:09:17Z jpuente $
------------------------------------------------------------------------------
-- Project GS - toy ground station with GUI
-- Main procedure body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with Parameters;
with GS_TM, GS_TC;
with GUI;       use GUI;

with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Sockets;     use GNAT.Sockets;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with System.IO;

-- usage: gs [OBSW_IP [OBSW_PORT [TM_PORT]]]
--
--   OBSW IP   : IP address of toy satellite on-board software
--   OBSW Port : Port where TC to the toy satelly are addressed
--   TM_Port   : local port where TM messages are received
--   see Parameters for default values
--
procedure GS is
begin

   if Argument_Count >= 1 then
      Parameters.OBSW_IP := Inet_Addr(Argument(1));
   end if;
   if Argument_Count >= 2 then
      Parameters.OBSW_Port := Port_Type(Integer'Value(Argument(2)));
   end if;
   if Argument_Count >= 3 then
      Parameters.TM_Port := Port_Type(Integer'Value(Argument(3)));
   end if;

   System.IO.Put_Line("--- GS start ---");
   -- initialize components
   GS_TM.Init;
   GS_TC.Init;
   GUI.Init;
   -- control returns here if the GUi window is closed
   System.IO.Put_Line("--- GS end ---");

end GS;

