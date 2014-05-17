Strict

Public

' Imports:
'Import sizeof

' Constant variable(s):

' Character codes:
Const QuoteChar:Int = 34

' Character strings:
Const Comma:String = ","
Const Space:String = " "
Const Slash:String = "/"
Const DotSlash:String = "./"
Const ColonSlash:String = ":/"
Const BackSlash:String = "\"
Const DotBackSlash:String = ".\"
Const ColonBackSlash:String = ":\"

' Global variable(s):
Global Quote:String = String.FromChar(QuoteChar)

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
	Local An:String = "an"
	
	If (LI.StartsWith("a")) Then Return An
	If (LI.StartsWith("e")) Then Return An
	If (LI.StartsWith("i")) Then Return An
	If (LI.StartsWith("o")) Then Return An
	If (LI.StartsWith("u")) Then Return An
	
	' Return the default response.
	Return "a"
End