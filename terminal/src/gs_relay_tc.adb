-- $Id: gs_relay_tc.adb 99 2017-05-25 14:12:55Z jpuente $
------------------------------------------------------------------------------
-- Project GS - toy ground station
-- GS- relay TC body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with GNAT.Sockets;     use GNAT.Sockets;
with Ada.Streams;

with System.IO;
with Ada.Exceptions;

package body GS_Relay_TC is

   -- Local port for receiving TC messages
   TC_Port   : Port_Type := 8484;

   -- receive socket
   RX_Socket   : Socket_Type;
   RX_Address  : Sock_Addr_Type;
   From        : Sock_Addr_Type;

   -- Remote port to which TC messages are relayed
   OBDH_IP    : Inet_Addr_Type := Inet_Addr("138.4.12.47");
   OBDH_Port  : Port_Type := 8484;

   -- transmit socket
   TX_Socket  : Socket_Type;
   TX_Address : Sock_Addr_Type;

   TM_Message_Size : Ada.Streams.Stream_Element_Offset := 512;
   subtype TM_Stream is
     Ada.Streams.Stream_Element_Array (1..TM_Message_Size);

   Data       : TM_Stream;
   Last       : Ada.Streams.Stream_Element_Offset;

   --------------
   -- TC_Relay --
   --------------

   task body TC_Relay is
   begin
      delay 0.200;
      --  TC receive socket
      Create_Socket (RX_Socket, Family_Inet, Socket_Datagram);
      RX_Address := (Family => Family_Inet,
                     Addr   => Any_Inet_Addr,
                     Port   => Port_Type(TC_Port));
      Bind_Socket (RX_Socket, RX_Address);

      System.IO.Put("TC listening on port " & Image(RX_Address));

      -- TC relay socket
      Create_Socket(TX_Socket, Family_Inet, Socket_Datagram);
      TX_Address := (Family_Inet, OBDH_IP, OBDH_Port);

      System.IO.Put_Line("; relaying to " & Image(TX_Address));

      loop
         begin
            Receive_Socket (RX_Socket, Data, Last, From);
            System.IO.Put_Line("..received TC from " & Image(RX_Address));
            Send_Socket(TX_Socket, Data, Last, TX_Address);
            System.IO.Put_Line("...relayed to " & Image(TX_Address));
         exception
            when E : Socket_Error =>
               System.IO.Put_Line(Ada.Exceptions.Exception_Message(E));
         end;
      end loop;

   exception
      when E : others =>
         Close_Socket (RX_Socket);
         Close_Socket (TX_Socket);
         System.IO.Put_Line("GS relay stopped "
                            & Ada.Exceptions.Exception_Message(E));
   end TC_Relay;

end GS_Relay_TC;
