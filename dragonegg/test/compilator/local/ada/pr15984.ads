package PR15984 is

   type Type1 is (AAA, BBB);

   type Type2 is record
      CCC : Boolean;
      DDD : Float;
   end record;
   pragma pack( Type2 );

   type Type3 (Format : Type1 := AAA) is record
      case Format is
   	 when AAA  =>	
   	    EEE : Boolean;
   	 when BBB  =>
   	    FFF : Type2;
      end case;
   end record;
   pragma PACK (Type3);

   procedure Startup;

   procedure Set;

   procedure Reset;

end PR15984;
