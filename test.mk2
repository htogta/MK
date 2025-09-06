
%offset 30

%def test-macro NOP .lbl1 ADD2 @lbl1 %end

HLT
.lbl
%pad 3
#33
@lbl?

%test-macro

%padto 0x33
#69 HLT
