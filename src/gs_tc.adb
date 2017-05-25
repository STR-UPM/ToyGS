------------------------------------------------------------------------------
-- $Id: gs_tm.ads 86 2017-05-04 14:09:17Z jpuente $
------------------------------------------------------------------------------
-- Project GS
-- GS_TC body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with Parameters;

with GNAT.Sockets;            use GNAT.Sockets;
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Streams;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

With System.IO;
with Ada.Exceptions;

package body GS_TC is

   ----------------------
   -- Data definitions --
   ----------------------

   type TC_Message(Kind : TC_Type) is
      record
         Timestamp : Time;
      end record;

   subtype  TC_Stream is Ada.Streams.Stream_Element_Array (1..TC_Message'Size);
   function To_Stream is new Ada.Unchecked_Conversion (TC_Message, TC_Stream);

   Socket         : Socket_Type;
   OBSW_Address   : Sock_Addr_Type;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      Create_Socket(Socket, Family_Inet, Socket_Datagram);
      OBSW_Address := (Family_Inet, Parameters.OBSW_IP, Parameters.OBSW_Port);
      pragma Debug
        (System.IO.Put_Line("... send TCs to " & Image(OBSW_Address)));
   end Init;

   ---------------------
   -- Send TC request --
   ---------------------

   procedure Send(TC : TC_Type := HK) is
      T       : Time;
      SC      : Seconds_Count;
      TS      : Time_Span;
   begin
      T := Clock;
      Split(T, SC, TS);
      declare
         Command : TC_Message := (Kind => TC, Timestamp => T);
         Stream  : TC_Stream  := To_Stream(Command);
         Last    : Ada.Streams.Stream_Element_Offset;
      begin
         Send_Socket(Socket, Stream, Last, OBSW_Address);
         pragma Debug
           (System.IO.Put_Line("TC " & SC'Img & " : " & TC'Img));
      end;
   exception
      when E : others =>
         Close_Socket (Socket);
         System.IO.Put_Line
           ("TC error " & Ada.Exceptions.Exception_Message(E));
   end Send;

end GS_TC;
