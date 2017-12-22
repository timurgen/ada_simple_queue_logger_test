with Logger;
procedure Main is

begin
   for I in 1 .. 10 loop
      Logger.Log("Ping " & I'Image, Logger.FATAL);
   end loop;
end Main;
