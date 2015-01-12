#Rem
	NOTES:
		NOMENCLATURE / OTHER:
			* Though I do use the formal plural forms of 'suffix' and 'prefix' in this module,
			I prefer the non-standard forms ('Prefices' and 'Suffices'),
			so you may see those two words creep in on occasion.
	TODO:
		* Add the option to use custom lengths and offsets for 'FindInString' and 'FindInStrings'.
#End

Strict

Public

' Preprocessor related:
#STRINGUTIL_IMPLEMENTED = True

' Enabling this will ensure that you won't have any compatibility issues.
#STRINGUTIL_MAXIMIZE_COMPATIBILITY = True ' False

#If STRINGUTIL_MAXIMIZE_COMPATIBILITY
	#STRING_UTIL_IMPLEMENTED = STRINGUTIL_IMPLEMENTED
	#STRINGUTIL_IMPORT_RETROSTRINGS = True
#Else
	#STRINGUTIL_IMPORT_RETROSTRINGS = False
#End

' If this is enabled commands like 'CleanString' will do extra work ensuring safety. (Partially implemented)
#STRINGUTIL_SAFE = True

' If disabled, a potentially faster, but overall less
' reliable method will be used to shorten floating-point strings.
#STRINGUTIL_ALTERNATE_FLOAT_SHORTEN = False

#Rem
	This allows you to toggle the use of enumerators in some situations.
	This is mainly for reading from arrays; this could provide a
	small performance boost on some targets.
	
	This preprocessor variable is a suggestion/guideline, not a rule.
	Not all routines support this feature.
#End

#If TARGET = "android" ' And Not STRINGUTIL_MAXIMIZE_COMPATIBILITY
	' Enumerators are not preferred on Android-based targets
	' due to the way they are commonly generated.
	#STRINGUTIL_PREFER_ENUMERATORS = False
#Else
	' On other targets, we shouldn't have any issues.
	#STRINGUTIL_PREFER_ENUMERATORS = True
#End

' Imports:
#If STRINGUTIL_IMPORT_RETROSTRINGS
	Import retrostrings
#End

' Constant variable(s):
Const STRING_INVALID_LOCATION:Int		= -1

Const STRING_SEARCH_OUTPUT_SIZE:Int		= 2

' String-search output-indexes (See 'FindInString' and 'FindInStrings' for details):
Const STRING_SEARCH_ARRAY_POSITION:Int			= 0
Const STRING_SEARCH_STR_POSITION:Int			= 1

#If STRINGUTIL_MAXIMIZE_COMPATIBILITY
	Const STRING_SEARCH_ARRAY:= STRING_SEARCH_ARRAY_POSITION
	Const STRING_SEARCH_STR_POS:= STRING_SEARCH_STR_POSITION
#End

' Character codes:
Const QuoteChar:Int = 34 ' "
Const AsteriskChar:Int = 42 ' *
Const QuestionMarkChar:Int = 63 ' ?

' Character strings:
Const Quote:String = "~q"
Const Asterisk:String = "*"
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
Function InQuotes:String(Input:String)
	Return Quote + Input + Quote
End

Function InQuotes:String(Input:String, QChar:Int)
	' Local variable(s):
	
	' Get the quote-string from the character specified.
	Local Q:= String.FromChar(QChar)
	
	' Return the processed string.
	Return Q + Input + Q
End

#Rem
	This command will get things right most of the time,
	but it doesn't have a dictionary, so it's not perfect.
	
	This really depends on how the word is pronounced,
	but at least vowels will always be properly handled.
#End

Function AOrAn:String(Input:String)
	' Constant variable(s):
	Const An:String = "an"
	Const A:String = "a"
	
	' Local variable(s):
	Local LI:= Input.ToLower()
	
	' Check for English vowels:
	If (LI.StartsWith("a")) Then Return An
	If (LI.StartsWith("e")) Then Return An
	If (LI.StartsWith("i")) Then Return An
	If (LI.StartsWith("o")) Then Return An
	If (LI.StartsWith("u")) Then Return An
	
	' Return the default response.
	Return A
End

Function BoolToString:String(In:Bool)
	If (In) Then
		Return "True"
	Endif
	
	Return "False"
End

' If 'STRINGUTIL_SAFE' is disabled, and the 'Precision' argument
' of this command is less than one, integer conversion will be used.
Function ShortenedFloat:String(F:Float, Precision:Int=1)
	#If Not STRINGUTIL_SAFE
		If (Precision < 1) Then
			Return String(Int(F))
		Endif
	#End
	
	#If Not STRINGUTIL_ALTERNATE_FLOAT_SHORTEN
		' Local variable(s):
		Local S_F:= String(F)
		
		#If STRINGUTIL_SAFE
			If (Precision < 1) Then
				Return S_F[..S_F.Find(Dot)]
			Endif
		#End
		
		Return S_F[..S_F.Find(Dot)+1+Precision]
	#Else
		#If STRINGUTIL_SAFE
			' This is done so we don't divide by zero.
			Precision = Max(Precision, 1)
		#End
		
		Local X:= Float(Pow(10, Precision))
		
		F *= X
		
		Return String(Int(F + (Sgn(F) * 0.5)) / X)
	#End
End

Function CleanString:String(Input:String, ReplaceStrs:String[]=["~n", "~r", "~t"]) ' [..., ~q"]
	For Local RS:= Eachin ReplaceStrs
		#If STRINGUTIL_SAFE
			If (RS.Length() > 0) Then
		#End
				' Replace each token with nothing.
				Input = Input.Replace(RS, "")
		#If STRINGUTIL_SAFE
			Endif
		#End
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
		Return FindInStrings(SA, Keys, Output)
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
	Local Position:= STRING_INVALID_LOCATION
	
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
		Return FindInString(S, Keys, ExitOnMatch)
	Endif
	
	Return Position
End

Function FindInString:Int(S:String, Keys:String[], ExitOnMatch:Bool)
	Return FindInString(S, Keys, "", "", ExitOnMatch)
End

Function FindInString:Int(S:String, Keys:String[], KeyPrefix:String, KeySuffix:String, ExitOnMatch:Bool=False)
	' Local variable(s):
	Local Position:= STRING_INVALID_LOCATION
	
	For Local I:= 0 Until Keys.Length()
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

Function FindInString:Int(S:String, Keys:String[], ExitOnMatch:Bool=False)
	' Local variable(s):
	Local Position:= STRING_INVALID_LOCATION
	
	For Local I:= 0 Until Keys.Length()
		Local P:= S.Find(Keys[I])
		
		If (P > Position) Then
			If (ExitOnMatch) Then
				Return P
			Endif
			
			Position = P
		Endif
	Next
	
	Return Position
End

Function FindInStrings:Int[](SA:String[], Keys:String[], Output:Int[]=[])
	For Local I:= 0 Until SA.Length()
		Local FindResponse:= FindInString(SA[I], Keys)
		
		If (FindResponse <> STRING_INVALID_LOCATION) Then
			If (Output.Length() = 0) Then
				Output = New Int[STRING_SEARCH_OUTPUT_SIZE]
			Endif
			
			Output[STRING_SEARCH_STR_POSITION] = FindResponse
			Output[STRING_SEARCH_ARRAY_POSITION] = I
			
			' Return the output-array.
			Return Output ' [I, FindResponse]
		Endif
	Next
	
	' Return an empty array.
	Return []
End

Function FindInStrings:Int[](SA:String[], Keys:String[], KeyPrefix:String, KeySuffix:String, Output:Int[]=[])
	For Local I:= 0 Until SA.Length()
		Local FindResponse:= FindInString(SA[I], Keys, KeyPrefix, KeySuffix)
		
		If (FindResponse <> STRING_INVALID_LOCATION) Then
			If (Output.Length() = 0) Then
				Output = New Int[STRING_SEARCH_OUTPUT_SIZE]
			Endif
			
			Output[STRING_SEARCH_STR_POSITION] = FindResponse
			Output[STRING_SEARCH_ARRAY_POSITION] = I
			
			' Return the output-array.
			Return Output ' [I, FindResponse]
		Endif
	Next
	
	' Return an empty array.
	Return []
End

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
	
	' Return an empty array.
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
	
	' Return an empty array.
	Return []
End

Function FindString_Suffixes:Int(S:String, Keys:String[], KeyPrefix:String, KeySuffixes:String[], ExitOnMatch:Bool=False)
	' Local variable(s):
	Local KeySuffixes_Length:= KeySuffixes.Length()
	
	' This will represent the position we find the key at.
	Local Position:= STRING_INVALID_LOCATION
	
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
	Local Position:= STRING_INVALID_LOCATION
	
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

Function StringStartsWith:Bool(S:String, Token:String)
	Return S.StartsWith(Token)
End

Function StringStartsWith:Bool(S:String, Tokens:String[])
	#If STRINGUTIL_PREFER_ENUMERATORS
		For Local Token:= Eachin Tokens
			If (Token.Length() = 0) Then
				Continue
			Endif
			
			'If (StringStartsWith(S, Token)) Then
			If (S.StartsWith(Token)) Then
				Return True
			Endif
		Next
	#Else
		For Local Index:= 0 Until Tokens.Length()
			Local Token:= Tokens[Index]
			
			If (Token.Length() = 0) Then
				Continue
			Endif
			
			'If (StringStartsWith(S, Token)) Then
			If (S.StartsWith(Token)) Then
				Return True
			Endif
		Next
	#End
	
	' Return the default response.
	Return False
End

' This command will return the index in the 'Tokens' array where it found a match.
' For a simple boolean check, just use the 'StringStartsWith' command.
Function StringStartsWith_Exact:Int(S:String, Tokens:String[])
	For Local Index:= 0 Until Tokens.Length()
		Local Token:= Tokens[Index]
		
		If (Token.Length() = 0) Then
			Continue
		Endif
		
		'If (StringStartsWith(S, Token)) Then
		If (S.StartsWith(Token)) Then
			Return Index
		Endif
	Next
	
	' Return the default response.
	Return STRING_INVALID_LOCATION
End

Function StringEndsWith:Bool(S:String, Token:String)
	Return S.EndsWith(Token)
End

Function StringEndsWith:Bool(S:String, Tokens:String[])
	#If STRINGUTIL_PREFER_ENUMERATORS
		For Local Token:= Eachin Tokens
			If (Token.Length() = 0) Then
				Continue
			Endif
			
			'If (StringEndsWith(S, Token)) Then
			If (S.EndsWith(Token)) Then
				Return True
			Endif
		Next
	#Else
		For Local Index:= 0 Until Tokens.Length()
			If (Token.Length() = 0) Then
				Continue
			Endif
			
			'If (StringEndsWith(S, Tokens[Index])) Then
			If (S.EndsWith(Tokens[Index])) Then
				Return True
			Endif
		Next
	#End
	
	' Return the default response.
	Return False
End

' This command will return the index in the 'Tokens' array where it found a match.
' For a simple boolean check, just use the 'StringEndsWith' command.
' This is effectively the same as the 'StringStartsWith_Exact' command.
Function StringEndsWith_Exact:Int(S:String, Tokens:String[])
	For Local Index:= 0 Until Tokens.Length()
		Local Token:= Tokens[Index]
		
		If (Token.Length() = 0) Then
			Continue
		Endif
		
		'If (StringEndsWith(S, Token)) Then
		If (S.EndsWith(Token)) Then
			Return Index
		Endif
	Next
	
	' Return the default response.
	Return STRING_INVALID_LOCATION
End

Function InvalidStringSearch:Bool(Response:Int)
	Return (Response = STRING_INVALID_LOCATION) ' Alternate version: (Response <= -1)
End

Function InvalidStringSearch:Bool(Response:Int[])
	Return (Response.Length() = 0 Or Response[STRING_SEARCH_ARRAY_POSITION] = STRING_INVALID_LOCATION Or Response[STRING_SEARCH_STR_POSITION] = STRING_INVALID_LOCATION)
End

#Rem
	This command is based on a function developed by Pharmhaus of the Monkey community.
	That implementation was a non-recursive derivative of Vishal Chaudhary's C implementation,
	presented on GeeksforGeeks.org (www.geeksforgeeks.org/wildcard-character-matching).
	
	Unlike Pharmhaus's implementation, however, this will treat
	multi-character wild-card symbols as an undefined set of characters (Potentially none).
	
	This is similar to what is used by the Windows File Explorer's search routine.
	
	Basically, this will allow you to compare standard strings with
	strings containing only part of the data, and see if the data is compatible.
	
	This is commonly used for search routines, most notably for file searches.
	
	The "_Position" arguments are used for offsetting the routine's "start-points",
	and the "_Length" arguments specify the "end-points" plus one (The lengths of the strings).
#End

Function WildCard:Bool(WildCardString:String, ToMatch:String, WildCardString_Position:Int=0, ToMatch_Position:Int=0)
	Return WildCard(WildCardString, ToMatch, WildCardString_Position, ToMatch_Position, WildCardString.Length(), ToMatch.Length())
End

Function WildCard:Bool(WildCardString:String, ToMatch:String, WildCardString_Position:Int, ToMatch_Position:Int, WildCardString_Length:Int, ToMatch_Length:Int)
	' Constant variable(s):
	Const WildCardChar_Multiple:= AsteriskChar ' 42
	Const WildCardChar_Single:= QuestionMarkChar ' 63
	
	' Local variable(s):
	Local WildCard_MultiCharSymbol_Position:Int = 0
	
	' Continue processing until 'WildCardString' and 'ToMatch' have been compared:
	While (WildCardString_Position < WildCardString_Length)
		' Check for the multi-character wild-card symbol:
		If (WildCardString[WildCardString_Position] = WildCardChar_Multiple) Then
			' Store the position of the current character, so we may
			' jump back to it if our later structural assumptions are incorrect.
			WildCard_MultiCharSymbol_Position = WildCardString_Position
			
			' Move past any extra multi-character wild-card symbols:
			While (WildCardString_Position < WildCardString_Length And WildCardString[WildCardString_Position] = WildCardChar_Multiple)
				WildCardString_Position += 1
			Wend
			
			' If we're at the end of 'WildCardString', return 'True'.
			' (We can safely assume the other string is fine):
			If (WildCardString_Position = WildCardString_Length) Then
				Return True
			Endif
			
			' Skip any ignored characters (Single-character wild-cards).
			' This does not apply to normal characters, however:
			While (WildCardString_Position < WildCardString_Length And WildCardString[WildCardString_Position] = WildCardChar_Single)
				WildCardString_Position += 1
				ToMatch_Position += 1
			Wend
			
			' If we ran over the limit, exit the main loop:
			If (WildCardString_Position = WildCardString_Length) Then
				Exit
			Endif
			
			' Local variable(s):
			
			#Rem
				Cache the current character of 'WildCardString', so we don't have to keep loading it.
				On some targets, this could be an unneeded optimization, but for the sake
				of keeping everything as fast as possible, I'm keeping this on the stack.
				
				It's not like this would hurt anything to begin with.
			#End
			
			Local WildCardString_CurrentChar:= WildCardString[WildCardString_Position]
			
			' Move past any characters that are unique to 'ToMatch':
			While (ToMatch_Position < ToMatch_Length And WildCardString_CurrentChar <> ToMatch[ToMatch_Position])
				ToMatch_Position += 1
			Wend
		Elseif (ToMatch_Position = ToMatch_Length) Then
			' We've made it to the end of 'ToMatch',
			' exit this loop, and perform any final tasks.
			Exit
		Elseif ((WildCardString[WildCardString_Position] = WildCardChar_Single Or WildCardString[WildCardString_Position] = ToMatch[ToMatch_Position])) Then
			#Rem
				We've found either the single-character wild-card symbol,
				or matching characters between the two strings,
				move forward through both strings by one character:
			#End
			
			WildCardString_Position += 1
			ToMatch_Position += 1
		Elseif (WildCardString_Position < WildCardString_Length) Then
			#Rem
				If this point was reached, we've made a mistake in pattern detection.
				Basically, the current characters do not match,
				unlike the previous characters we've detected.
				
				So, we need to move our position in 'WildCardString' back to where
				we last found a multi-character wild-card symbol,
				but the position in 'ToMatch' will stay the same.
				This means that we won't get the same results we just had.
			#End
			
			WildCardString_Position = WildCard_MultiCharSymbol_Position
		Else
			' If this point was reached, we already know that
			' the two strings are not compatible, tell the user.
			Return False
		Endif
	Wend
	
	' If nothing else, tell the user if we're at the end of both strings or not.
	Return (WildCardString_Position = WildCardString_Length And ToMatch_Position = ToMatch_Length)
End

' Functions (Private):
Private

' Nothing so far.

Public

' Classes:
Class StringException Extends Throwable
	' Constant variable(s):
	Const ErrorTemplate:String = "{STRING ERORR}: "
	Const Default_ErrorMessage:String = ErrorTemplate + "An unknown has occurred."
	
	' Constructor(s):
	Method New(Message:String=Default_ErrorMessage)
		Self.Message = Message
	End
	
	' Methods:
	' Nothing so far.
	
	' Properties:
	Method ToString:String() Property
		Return Message
	End
	
	' Fields:
	Field Message:String
End

Class StringContentException Extends StringException
	' Constant variable(s):
	Const ErrorSeparator:String = " :: "
	
	' Constructor(s):
	Method New(Message:String=Default_ErrorMessage, Content:String="")
		' Call the super-class's implementation.
		Super.New(Message)
		
		' Assign the internal-string which caused the error.
		Self.Content = Content
	End
	
	' Properties:
	Method ToString:String() Property
		Return Super.ToString() + ErrorSeparator + Content
	End
	
	' Fields:
	Field Content:String
End