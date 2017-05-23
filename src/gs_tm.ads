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
with Measurements;
with Ada.Real_Time;

package GS_TM is
   use Measurements, Ada.Real_Time;

   type TM_Type is (Basic, HK);

   type TM_Message (Kind : TM_Type) is
      record
         Timestamp : Time;
         case Kind is
            when Basic =>
               Data  : Measurement;
            when HK =>
               Data_Log  : HK_Data;
               Length    : Positive;
         end case;
      end record;

private
   task Receiver;
end GS_TM;
