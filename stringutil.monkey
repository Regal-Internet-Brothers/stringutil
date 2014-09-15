#Rem
	NOTES:
		NOMENCLATURE / OTHER:
			* Though I do use the formal plural forms of 'suffix' and 'prefix' in this module,
			I prefer the non-standard forms ('Prefices' and 'Suffices'),
			so you may see those two words creep in on occasion.
#End

Strict

Public

' Preprocessor related:
#STRING_UTIL_IMPLEMENTED = True

' Imports:
Import retrostrings

' Constant variable(s):
Const STRING_INVALID_LOCATION:Int		= -1

Const STRING_SEARCH_OUTPUT_SIZE:Int		= 2

' String-search output-indexes (See 'FindInString' and 'FindInStrings' for details):
Const STRING_SEARCH_ARRAY:Int			= 0
Const STRING_SEARCH_STR_POS:Int			= 1

' Character codes:
Const QuoteChar:Int = 34

' Character strings:
Const Quote:String = "~q"
Const Comma:String = ","
Const Colon:String = ":"
Const Dash:String = "-"
Const Plus:String = "+"
Const QuestionMark:String = "?"
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

' Global variable(s):
' Nothing so far.

' Functions (Public):
Function InQuotes:String(Input:String, QChar:Int=QuoteChar)
	' Local variable(s):
	
	' Get the quote-string from the character specified.
	Local Q:= String.FromChar(QChar)
	
	' Return the processed string.
	Return Q + Input + Q
End

' This command will get things right most of the time,
' but it doesn't have a dictionary, so it's not perfect about vowels.
Function AOrAn:String(Input:String)
	' Constant variable(s):
	Const An:String = "an"
	
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

Function CleanString:String(Input:String, ReplaceStrs:String[]=["~n", "~t"])
	For Local RS:= Eachin ReplaceStrs
		If (RS.Length() > 0) Then
			' Replace each token with nothing.
			Input = Input.Replace(RS, "")
		Endif
	Next
	
	' Return the newly processed input-string and treat it as an output.
	Return Input
End

' The following string search routines are for searching with arrays; if you just need to search for one string, use the default 'Find' method:

' This isn't the best of ways to use 'FindInStrings', please use that directly.
Function FindInString:Int[](SA:String[], Keys:String[], KeyPrefix:String="", KeySuffix:String="", Output:Int[]=[])
	Return FindInStrings(SA, Keys, KeyPrefix, KeySuffix, Output)
End

' This command can take empty arrays for prefixes and/or suffixes if needed.
Function FindInStrings:Int[](SA:String[], Keys:String[], KeyPrefixes:String[], KeySuffixes:String[]=[], Output:Int[]=[])
	' Local variable(s):
	
	' Array length caches (Could be more optimal, but this will do):
	Local KeyPrefixes_Length:= KeyPrefixes.Length()
	Local KeySuffixes_Length:= KeySuffixes.Length()
	
	' See what we can do about the input-arrays:
	If (KeyPrefixes_Length > 0) Then
		' We have prefixes, so we need to send in the suffixes, and iterate through the prefixes:
		For Local KP_I:= 0 Until KeyPrefixes_Length
			' Get the data supplied from the suffix-loop.
			Local Data:= FindInStrings_Suffixes(SA, Keys, KeyPrefixes[KP_I], KeySuffixes, Output)
			
			' If data was found, we can return it.
			If (Data.Length() > 0) Then
				' We're good, return the data supplied.
				Return Data
			Endif
		Next
	Elseif (KeySuffixes_Length > 0) Then
		' We have suffixes, so we need to send in the prefixes, and iterate through the suffixes:
		For Local KS_I:= 0 Until KeySuffixes_Length
			' Get the data supplied from the prefix-loop.
			Local Data:= FindInStrings_Prefixes(SA, Keys, KeyPrefixes, KeySuffixes[KS_I], Output)
			
			' If data was found, we can return it.
			If (Data.Length() > 0) Then
				' We're good, return the data supplied.
				Return Data
			Endif
		Next
	Else
		' If nothing else, call the version without prefixes and suffixes.
		Return FindInStrings(SA, Keys, "", "", Output)
	Endif
	
	' If we get to this point, nothing could be found.
	Return []
End

' This command can take empty arrays for prefixes and/or suffixes if needed.
Function FindInString:Int(S:String, Keys:String[], KeyPrefixes:String[], KeySuffixes:String[]=[], ExitOnMatch:Bool=False)
	' Local variable(s):
	
	' Array length caches (Could be more optimal, but this will do):
	Local KeyPrefixes_Length:= KeyPrefixes.Length()
	Local KeySuffixes_Length:= KeySuffixes.Length()
	
	' This will represent the position we find the key at.
	Local Position:Int = STRING_INVALID_LOCATION
	
	' See what we can do with the input-arrays:
	If (KeyPrefixes_Length > 0) Then
		' We have prefixes, so we need to send in the suffixes, and iterate through the prefixes:
		For Local KP_I:= 0 Until KeyPrefixes_Length
			Local P:= FindString_Suffixes(S, Keys, KeyPrefixes[KP_I], KeySuffixes, ExitOnMatch)
			
			If (P > Position) Then
				If (ExitOnMatch) Then
					Return P
				Endif
				
				Position = P
			Endif
		Next
	Elseif (KeySuffixes_Length > 0) Then
		' We have suffixes, so we need to send in the prefixes, and iterate through the suffixes:
		For Local KS_I:= 0 Until KeySuffixes_Length
			Local P:= FindString_Prefixes(S, Keys, KeyPrefixes, KeySuffixes[KS_I], ExitOnMatch)
			
			If (P > Position) Then
				If (ExitOnMatch) Then
					Return P
				Endif
				
				Position = P
			Endif
		Next
	Else
		' If we couldn't use the input arrays, we need to forget them, instead of just exiting.
		Return FindInString(S, Keys, "", "", ExitOnMatch)
	Endif
	
	Return Position
End

Function FindInString:Int(S:String, Keys:String[], ExitOnMatch:Bool)
	Return FindInString(S, Keys, "", "", ExitOnMatch)
End

Function FindInString:Int(S:String, Keys:String[], KeyPrefix:String="", KeySuffix:String="", ExitOnMatch:Bool=False)
	Local Position:Int = STRING_INVALID_LOCATION
	
	For Local I:Int = 0 Until Keys.Length()
		Local P:= S.Find(KeyPrefix + Keys[I] + KeySuffix)
		
		If (P > Position) Then
			If (ExitOnMatch) Then
				Return P
			Endif
			
			Position = P
		Endif
	Next
	
	Return Position
End

Function FindInStrings:Int[](SA:String[], Keys:String[], KeyPrefix:String="", KeySuffix:String="", Output:Int[]=[])
	For Local I:Int = 0 Until SA.Length()
		Local FindResponse:= FindInString(SA[I], Keys, KeyPrefix, KeySuffix)
		
		If (FindResponse <> STRING_INVALID_LOCATION) Then
			If (Output.Length() = 0) Then
				Output = New Int[STRING_SEARCH_OUTPUT_SIZE]
			Endif
			
			Output[STRING_SEARCH_STR_POS] = FindResponse
			Output[STRING_SEARCH_ARRAY] = I
			
			' Return the output-array.
			Return Output ' [I, FindResponse]
		Endif
	Next
	
	' Return an empty array.
	Return []
End

Function InvalidStringSearch:Bool(Response:Int)
	Return (Response = STRING_INVALID_LOCATION) ' Alternate: (Response <= -1)
End

Function InvalidStringSearch:Bool(Response:Int[])
	Return (Response.Length() = 0 Or Response[STRING_SEARCH_ARRAY] = STRING_INVALID_LOCATION Or Response[STRING_SEARCH_STR_POS] = STRING_INVALID_LOCATION)
End

' Functions (Private):
Private

' Internal utility functions:

' These commands are here so you can pass in arguments for prefixes and suffixes applied as blank,
' without missing the entire point of calling the search-functions to begin with:
Function FindInStrings_Prefixes:Int[](SA:String[], Keys:String[], KeyPrefixes:String[], KeySuffix:String, Output:Int[]=[])
	' Local variable(s):
	Local KeyPrefixes_Length:= KeyPrefixes.Length()
	
	If (KeyPrefixes_Length > 0) Then
		For Local KP_I:= 0 Until KeyPrefixes_Length
			Local Data:= FindInStrings(SA, Keys, KeyPrefixes[KP_I], KeySuffix, Output)
			
			If (Data.Length() > 0) Then
				Return Data
			Endif
		Next
	Else
		Return FindInStrings(SA, Keys, "", KeySuffix, Output)
	Endif
	
	Return []
End

Function FindInStrings_Suffixes:Int[](SA:String[], Keys:String[], KeyPrefix:String, KeySuffixes:String[], Output:Int[]=[])
	' Local variable(s):
	Local KeySuffixes_Length:= KeySuffixes.Length()
	
	If (KeySuffixes_Length > 0) Then
		For Local KS_I:= 0 Until KeySuffixes_Length
			Local Data:= FindInStrings(SA, Keys, KeyPrefix, KeySuffixes[KS_I], Output)
			
			If (Data.Length() > 0) Then
				Return Data
			Endif
		Next
	Else
		Return FindInStrings(SA, Keys, KeyPrefix, "", Output)
	Endif
	
	Return []
End

Function FindString_Suffixes:Int(S:String, Keys:String[], KeyPrefix:String, KeySuffixes:String[], ExitOnMatch:Bool=False)
	' Local variable(s):
	Local KeySuffixes_Length:= KeySuffixes.Length()
	
	' This will represent the position we find the key at.
	Local Position:Int = STRING_INVALID_LOCATION
	
	If (KeySuffixes_Length > 0) Then
		For Local KS_I:= 0 Until KeySuffixes_Length
			Local P:= FindInString(S, Keys, KeyPrefix, KeySuffixes[KS_I], ExitOnMatch)
			
			If (P > Position) Then
				If (ExitOnMatch) Then
					Return P
				Endif
				
				Position = P
			Endif
		Next
	Else
		Return FindInString(S, Keys, KeyPrefix, "", ExitOnMatch)
	Endif
	
	Return Position
End

Function FindString_Prefixes:Int(S:String, Keys:String[], KeyPrefixes:String[], KeySuffix:String, ExitOnMatch:Bool=False)
	' Local variable(s):
	Local KeyPrefixes_Length:= KeyPrefixes.Length()
	
	' This will represent the position we find the key at.
	Local Position:Int = STRING_INVALID_LOCATION
	
	If (KeyPrefixes_Length > 0) Then
		For Local KP_I:= 0 Until KeyPrefixes_Length
			Local P:= FindInString(S, Keys, KeyPrefixes[KP_I], KeySuffix, ExitOnMatch)
			
			If (P > Position) Then
				If (ExitOnMatch) Then
					Return P
				Endif
				
				Position = P
			Endif
		Next
	Else
		Return FindInString(S, Keys, "", KeySuffix, ExitOnMatch)
	Endif
	
	Return Position
End

Public