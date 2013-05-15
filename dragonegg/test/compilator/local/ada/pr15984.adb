package body PR15984 is

  AA : Type3;

  procedure Startup is
  begin -- Startup
    AA.EEE := False;
  end Startup;

  procedure Set is
  begin -- Set
     AA.EEE := True;
  end Set;

  procedure Reset is
  begin -- Reset
     AA.EEE := False;
  end Reset;

end PR15984;
