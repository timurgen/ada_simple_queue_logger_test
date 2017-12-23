with Logger;
procedure Main is

begin
   Logger.Init;
   for I in 1 .. 2 loop
      Logger.Log("Ping " & I'Image, Logger.FATAL);
   end loop;
   Logger.Stop;
end Main;
