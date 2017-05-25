------------------------------------------------------------------------------
-- $Id: gs.adb 86 2017-05-04 14:09:17Z jpuente $
------------------------------------------------------------------------------
-- Project GS - toy ground station
-- Gui_Callbacks specification
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------

-- Graphic user interface
--
package GUI is

   -- Init GUI and launch window
   procedure Init;

   -- Put TM message on message are
   procedure Put_TM (Message : String);

end GUI;
