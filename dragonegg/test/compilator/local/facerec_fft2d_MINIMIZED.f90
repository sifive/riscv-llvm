	Module FFT2D 
	Contains
	Subroutine FFT2DF (Space, Freq)
	Complex(4), Intent(In) :: Space (:, :)
	Complex(4), Pointer    :: Freq  (:, :)
	Integer :: I, SpaceShape (2)
	SpaceShape = SHAPE (Space)
	If (ASSOCIATED (Freq)) Then
	  If (SpaceShape (1) /= SIZE (Freq, 2) .OR. &
	&     SpaceShape (2) /= SIZE (Freq, 1)) Then
          End If
	End If
	Do I = 1, ColLen
	End Do
	End Subroutine FFT2DF
	Subroutine FFT2DB (Freq, Space)
	Complex(4), Pointer     :: Space (:, :)
	If (ASSOCIATED (Space)) Then
	  If (FreqShape (1) /= SIZE (Space, 2) .OR. &
	&     FreqShape (2) /= SIZE (Space, 1)) Then
	  End If
	End If
	End Subroutine FFT2DB
	End Module FFT2D 
