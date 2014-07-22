Strict

Public

' Imports:
Import retrostrings

' Constant variable(s):

' Character codes:
Const QuoteChar:Int = 34

' Character strings:
Const Quote:String = "~q"
Const Comma:String = ","
Const LeftBracket:String = "["
Const RightBracket:String = "]"
Const Dot:String = "."
Const Space:String = " "
Const Slash:String = "/"
Const DotSlash:String = "./"
Const ColonSlash:String = ":/"
Const BackSlash:String = "\"
Const DotBackSlash:String = ".\"
Const ColonBackSlash:String = ":\"
Const SingleQuote:String = "'"

Const An:String = "an"

' Global variable(s):
' Nothing so far.

' Functions:
Function InQuotes:String(Input:String, QChar:Int=QuoteChar)
	' Local variable(s):
	
	' Get the quote-string from the character specified.
	Local Q:= String.FromChar(QChar)
	
	' Return the processed string.
	Return Q + Input + Q
End

Function AOrAn:String(Input:String)
	' Local variable(s):
	Local LI:= Input.ToLower()
	
	' Check for English vowels:
	If (LI.StartsWith("a")) Then Return An
	If (LI.StartsWith("e")) Then Return An
	If (LI.StartsWith("i")) Then Return An
	If (LI.StartsWith("o")) Then Return An
	If (LI.StartsWith("u")) Then Return An
	
	' Return the default response.
	Return "a"
End

Function BoolToString:String(In:Bool)
	If (In) Then Return "True"
	
	Return "False"
End

Function ShortenedFloat:String(F:Float, Precision:Int=1)
	' Local variable(s):
	Local S_F:= String(F)
	Local PrecisionStr:String = Right(S_F, Len(S_F) - Instr(S_F, "."))
	
	If (Precision < 1) Then
		PrecisionStr = "0"
		Precision = 1
	Endif
	
	Return Left(S_F, Instr(S_F, ".")) + Left(PrecisionStr, Precision)
End