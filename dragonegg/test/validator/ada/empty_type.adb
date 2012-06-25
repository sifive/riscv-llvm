-- RUN: %dragonegg -S -O1 %s
procedure Empty_Type is
   type Rec (Disc : Integer := 1) is
   record
      case Disc is
         when 1..0 => -- Empty range
            Component : Integer;
         when others => NULL;
      end case;
   end record;
   R : Rec;
begin
   null;
end;
