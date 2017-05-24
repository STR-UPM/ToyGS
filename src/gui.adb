------------------------------------------------------------------------------
-- $Id: gs.adb 86 2017-05-04 14:09:17Z jpuente $
------------------------------------------------------------------------------
-- Project GS - toy ground station
-- Gui_Callbacks body
-- Copyright (c) 2017 Juan Antonio de la Puente <jpuente@dit.upm.es>
-- Permission to copy and modify are granted under the terms of
-- the GNU General Public License (GPL).
-- See http://www.gnu.org/licenses/licenses.html#GPL for the details
------------------------------------------------------------------------------
with Gdk.Threads;
with Gtk.Main;
with Gtk.Window;          use Gtk.Window;
with Gtk.Grid;            use Gtk.Grid;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Button;          use Gtk.Button;
with Gtk.Label;           use Gtk.Label;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;

package body GUI is

   ---------------
   -- main_quit --
   ---------------

   procedure main_quit (Self : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end main_quit;

   ----------
   -- Init --
   ----------

   procedure Init is
      Window      : Gtk_Window;
      Grid        : Gtk_Grid;
      Label       : Gtk_Label;
      Button      : Gtk_Button;
      Scrolled    : Gtk_Scrolled_Window;
      Text_Buffer : Gtk_Text_Buffer;
      Text        : Gtk_Text_View;
   begin
      -- use thread-aware gdk
      Gdk.Threads.G_Init;
      Gdk.Threads.Init;
      Gtk.Main.Init;

      -- create window
      Gtk_New(Window);
      Window.Set_Title("Toy Satellite Ground Station");
      Window.Set_Border_Width (10);
      Window.Set_Resizable (False);
      Window.On_Destroy (main_quit'Access);

      -- grid for placing widgets
      Gtk_New (Grid);
      Window.Add(Grid);

      -- TM panel
      Gtk_New(Label, "Telemetry");
      Grid.Attach(Label, 0, 0, 4, 1);

      Gtk_New(Text_Buffer);
      Gtk_New(Text, Text_Buffer);
      --Text.Set_Editable(False);

      Gtk_New(Scrolled);
      Scrolled.Set_Policy(Policy_Automatic, Policy_Automatic);
      Scrolled.Set_Size_Request(60,200);
      Scrolled.Add(Text);
      Grid.Attach(Scrolled, 0,1,4,8);

      Text_Buffer.Insert_At_Cursor("sample");

      -- TC panel
      Gtk_New(Label, "Telecommands");
      Grid.Attach(Label, 4, 0, 1, 1);
      Gtk_New(Button, "Send TC");
      Grid.Attach(Button, 4,1,1,1);

      -- show window
      Grid.Set_Column_Homogeneous(True);
      Grid.Set_Column_Spacing(10);
      Grid.Set_Row_Spacing(10);
      Window.Show_All;

      -- GTK main loop
      Gdk.Threads.Enter;
      Gtk.Main.Main;
      Gdk.Threads.Leave;
   end Init;

end GUI;
