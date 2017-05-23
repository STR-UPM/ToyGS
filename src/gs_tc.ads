------------------------------------------------------------------------------
-- Project GS
-- GS_TC specification
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with Ada.Real_Time;
package GS_TC is
   use Ada.Real_Time;

   type TC_Type is (HK);

   type TC_Message(Kind : TC_Type) is
      record
         Timestamp : Time;
      end record;

private
   task Sender;
end GS_TC;
